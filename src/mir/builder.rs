use std::rc::Rc;

use index_vec::{index_vec, IndexVec};

use crate::{
    ast::{
        DeclItem, Expr, FloatSize, InfixOpKind, IntSize, PrefixOpKind, Restrictness, Signature,
        Signness, Ty, TyKind, Ty_, VarSpecifier,
    },
    error::{Error, ErrorReporter, ExprNotAllowedReason, Span, Spanned, ToSpanned},
    mir::NumLiteralContent,
    token::NumValue,
    utils::{fixme, IdentStr},
};

use super::{
    BlockId, FuncData, MirBlock, MirFunction, MirInst, MirTerm, NamesContext, Place,
    PlaceProjection, Value, VarId, VarInfo,
};

/// `Expr::Error` should not occur in this stage of the compilation.
macro panic_expr_error {
    ($span:expr) => {
        panic!("Expr::Error in MIR building stage (span: {:?})", $span)
    },
    () => {
        panic!("Expr::Error in MIR building stage")
    },
}

pub fn handle_top_level(
    cx: &mut NamesContext,
    stmt: Spanned<Expr>,
    err_reporter: Rc<ErrorReporter>,
) {
    let (stmt, span) = stmt.into_pair();
    dbg!(&stmt);
    err_reporter.exit_if_has_error();
    match stmt {
        Expr::Decl(item) => make_mir_for_item(cx, item, err_reporter.clone()),
        Expr::DeclList(items) => {
            for item in items {
                make_mir_for_item(cx, item, err_reporter.clone());
            }
        }
        Expr::EmptyDecl(_) => todo!(),
        _ => {
            err_reporter.report(
                &Error::ExprNotAllowed(ExprNotAllowedReason::InvalidTopLevelStmt).to_spanned(span),
            );
        }
    }
}

pub fn make_mir_for_item(cx: &mut NamesContext, item: DeclItem, err_reporter: Rc<ErrorReporter>) {
    match item {
        DeclItem::Func(func_speci, sig, name, body) => {
            cx.funcs.insert(
                *name,
                FuncData {
                    is_static: func_speci.linkage.is_staic(),
                    sig: sig.clone(),
                    mir_func: None,
                },
            );
            if let Some(body) = body {
                cx.locals.enters_block();
                let mut func_builder = MirFuncBuilder::new(cx, err_reporter, *name, sig);
                for stmt in body {
                    func_builder.build_stmt(stmt);
                }
                cx.funcs.get_mut(&name).unwrap().mir_func = Some(func_builder.mir_func);
                cx.locals.leaves_block();
            }
        }
        _ => todo!(),
    }
}

struct MirFuncBuilder<'cx> {
    cx: &'cx mut NamesContext,
    err_reporter: Rc<ErrorReporter>,
    mir_func: MirFunction,
    /// The "cursor" for writing new instructions onto.
    current_block: BlockId,
    func_name: IdentStr,
}

impl<'cx> MirFuncBuilder<'cx> {
    /// Function itself must have already been added to global context.
    /// Requires calling `GlobalContext::{enters_block, leaves_block}` outside.
    pub fn new(
        cx: &'cx mut NamesContext,
        err_reporter: Rc<ErrorReporter>,
        func_name: IdentStr,
        sig: Signature,
    ) -> Self {
        let vars: IndexVec<VarId, VarInfo> = sig
            .args
            .iter()
            .map(|&(ref ty, name)| VarInfo {
                is_arg: true,
                ty: ty.clone(),
                name,
                is_tmp: false,
            })
            .collect();
        let args: Vec<VarId> = vars.indices().collect();
        let mir_func = MirFunction {
            args,
            ret: sig.ret_ty.into_inner(),
            vars,
            blocks: index_vec![MirBlock::default()],
        };
        let self_ = Self {
            cx,
            err_reporter,
            mir_func,
            current_block: BlockId(0),
            func_name,
        };
        // Declare argument variables in context.
        for (id, info) in self_.mir_func.vars.iter_enumerated() {
            // Have to manually inline `add_var_to_cx` because partial borrow.
            if let Some(name) = info.name {
                _ = self_.cx.locals.add_var(*name, id).inspect_err(|()| {
                    self_
                        .err_reporter
                        .report(&Error::RedefinitionOfVar(*name).to_spanned(name.span()))
                });
            }
        }
        self_
    }
    fn add_inst(&mut self, inst: MirInst) {
        let current_block = &mut self.mir_func.blocks[self.current_block];
        if current_block.is_terminated {
            current_block.unreachable_insts.push(inst);
        } else {
            if matches!(inst, MirInst::Term(..)) {
                current_block.is_terminated = true;
            }
            current_block.insts.push(inst);
        }
    }
    fn add_var_to_cx(&mut self, name: Spanned<IdentStr>, id: VarId) -> Option<()> {
        self.cx
            .locals
            .add_var(*name, id)
            .inspect_err(|()| {
                self.err_reporter
                    .report(&Error::RedefinitionOfVar(*name).to_spanned(name.span()))
            })
            .ok()?;
        Some(())
    }

    fn declare_var(
        &mut self,
        ty: Ty,
        name: Option<Spanned<IdentStr>>,
        is_tmp: bool,
    ) -> Option<VarId> {
        let var_info = VarInfo {
            is_arg: false,
            ty,
            name,
            is_tmp,
        };
        let var_id = self.mir_func.vars.push(var_info);
        if let Some(name) = name {
            self.add_var_to_cx(name, var_id)?;
        }
        Some(var_id)
    }
    /// If types are unequal, elaborates typecast or report error.
    fn use_value(
        &mut self,
        expect_ty: Ty,
        (value, value_ty): (Value, Ty),
        span: Span,
    ) -> Option<(Value, Ty)> {
        if &value_ty.kind == &expect_ty.kind {
            return Some((value, value_ty));
        }
        if value_ty.is_arr() || expect_ty.is_arr() {
            todo!("arrays");
        }
        // Compile-time implicit cast for numeric types.
        if let &Value::Num(num) = &value {
            match &expect_ty.kind {
                TyKind::Int(..) => {
                    let num = match num {
                        NumLiteralContent::U(..) | NumLiteralContent::I(..) => Value::Num(num),
                        NumLiteralContent::F(f) if f.is_sign_positive() => {
                            let u = f.round() as u64;
                            Value::Num(u.into())
                        }
                        NumLiteralContent::F(f) if f.is_sign_negative() => {
                            let i = f.round() as i64;
                            Value::Num(i.into())
                        }
                        _ => unreachable!(),
                    };
                    return Some((num, expect_ty));
                }
                TyKind::Bool => {
                    let is_true = match num {
                        NumLiteralContent::U(u) => u != 0,
                        NumLiteralContent::I(i) => i != 0,
                        NumLiteralContent::F(f) => f != 0.0,
                    };
                    let num: u64 = if is_true { 1 } else { 0 };
                    return Some((Value::Num(num.into()), expect_ty));
                }
                TyKind::Float(..) => {
                    let num = match num {
                        NumLiteralContent::U(u) => Value::Num((u as f64).into()),
                        NumLiteralContent::I(i) => Value::Num((i as f64).into()),
                        f @ NumLiteralContent::F(..) => Value::Num(f),
                    };
                    return Some((num, expect_ty));
                }
                TyKind::Ptr(..) => match num {
                    NumLiteralContent::U(..) | NumLiteralContent::I(..) => {
                        return Some((Value::Num(num), expect_ty));
                    }
                    _ => (),
                },
                _ => (),
            }
        }
        // We already checked earlier that expect_ty does not equal to value_ty.
        if expect_ty.is_arithmatic_or_ptr() && value_ty.is_arithmatic_or_ptr() {
            let var_id = self.declare_var(expect_ty.clone(), None, true)?;
            // If both are pointers, ignore `const`/`volatile`/`restrict` of the pointer.
            if let (TyKind::Ptr(_, lhs_ty), TyKind::Ptr(_, rhs_ty)) =
                (&expect_ty.kind, &value_ty.kind)
            {
                // Assigning `T *` to `const T *` is a warning, smh.
                if !lhs_ty.is_const && rhs_ty.is_const {
                    fixme!("Warning for using const pointer as non-const pointer");
                }
            }
            self.add_inst(MirInst::Tycast(
                var_id.into(),
                expect_ty.clone(),
                value_ty,
                value,
            ));
            Some((Value::CopyPlace(var_id.into()), expect_ty))
        } else {
            self.err_reporter
                .report(&Error::MismatchedType(&expect_ty, &value_ty).to_spanned(span));
            None
        }
    }
    fn build_num_literal(&self, num: NumValue, span: Span) -> Option<(NumLiteralContent, Ty)> {
        match num {
            crate::token::NumValue::I(i) => {
                if let Ok(u) = u64::try_from(i) {
                    let size = if u32::try_from(u).is_ok() {
                        IntSize::_32
                    } else {
                        IntSize::_64
                    };
                    Some((
                        NumLiteralContent::U(u),
                        TyKind::Int(Signness::Unsigned, size).to_ty(true, false, None),
                    ))
                } else if let Ok(i) = i64::try_from(i) {
                    let size = if i32::try_from(i).is_ok() {
                        IntSize::_32
                    } else {
                        IntSize::_64
                    };
                    Some((
                        NumLiteralContent::I(i),
                        TyKind::Int(Signness::Signed, size).to_ty(true, false, None),
                    ))
                } else {
                    self.err_reporter
                        .report(&Error::OutOfRangeNumber(i).to_spanned(span));
                    None
                }
            }
            crate::token::NumValue::F(f) => Some((
                NumLiteralContent::F(f),
                TyKind::Float(FloatSize::_64).to_ty(true, false, None),
            )),
        }
    }
    fn build_unary_add(&mut self, operand: Spanned<Expr>) -> Option<(Value, Ty)> {
        let span = operand.span();
        let (value, ty) = self.build_expr(operand)?;
        if !matches!(&ty.kind, TyKind::Int(..) | TyKind::Float(..) | TyKind::Bool) {
            self.err_reporter
                .report(&Error::NonNumericInUnary.to_spanned(span));
            return None;
        }
        Some((value, ty))
    }
    fn build_unary_sub(&mut self, operand: Spanned<Expr>) -> Option<(Value, Ty)> {
        let (operand, span) = operand.into_pair();
        match operand {
            Expr::Error => panic_expr_error!(span),
            Expr::NumLiteral(num) => {
                let (num, mut ty) = self.build_num_literal(num, span)?;
                let num = match num {
                    NumLiteralContent::U(u) => {
                        ty = match ty.kind {
                            TyKind::Int(_, size) => {
                                TyKind::Int(Signness::Signed, size).to_ty(true, false, None)
                            }
                            _ => unreachable!(),
                        };
                        NumLiteralContent::I(-i64::from_be_bytes(u.to_be_bytes()))
                    }
                    NumLiteralContent::I(i) => NumLiteralContent::I(-i),
                    NumLiteralContent::F(f) => NumLiteralContent::F(-f),
                };
                Some((Value::Num(num), ty))
            }
            expr => {
                let (_value, ty) = self.build_expr(expr.to_spanned(span))?;
                if !matches!(&ty.kind, TyKind::Int(..) | TyKind::Float(..) | TyKind::Bool) {
                    self.err_reporter
                        .report(&Error::NonNumericInUnary.to_spanned(span));
                    return None;
                }
                todo!()
            }
        }
    }
    fn build_decl(&mut self, item: Spanned<DeclItem>) -> Option<()> {
        let (item, span) = item.into_pair();
        match item {
            DeclItem::Var(VarSpecifier::Typedef, _, _, None) => Some(()),
            DeclItem::Var(VarSpecifier::Typedef, _, _, Some(_rhs)) => {
                self.err_reporter
                    .report(&Error::TypedefWithRhs.to_spanned(span));
                None
            }
            // `register` storage specifier is ignored.
            DeclItem::Var(
                VarSpecifier::Auto | VarSpecifier::Register | VarSpecifier::Unspecified,
                ty,
                name,
                rhs,
            ) => {
                let var_id = self.declare_var(ty.into_inner(), Some(name), false)?;
                if let Some(rhs) = rhs {
                    // Have to manually re-write part of `build_assign` here because partial borrow.
                    let place = Place::from(var_id).to_spanned(name.span());
                    let rhs_span = rhs.span();
                    let rhs = self.build_expr(rhs.into_unboxed())?;
                    let lhs_ty = self.mir_func.vars[var_id].ty.clone();
                    if lhs_ty.is_void() || lhs_ty.is_const {
                        self.err_reporter
                            .report(&Error::ExprNoAssignable.to_spanned(span));
                        return None;
                    }
                    let value = self.use_value(lhs_ty, rhs, rhs_span)?.0;
                    self.add_inst(MirInst::Assign(place.into_inner(), value));
                }
                Some(())
            }
            DeclItem::Var(VarSpecifier::Extern | VarSpecifier::Static, ..) => {
                todo!("static/extern variables")
            }
            DeclItem::Func(..) => {
                self.err_reporter.report(
                    &Error::ExprNotAllowed(ExprNotAllowedReason::NestedFunc).to_spanned(span),
                );
                None
            }
        }
    }
    fn build_call(
        &mut self,
        callee: Spanned<Expr>,
        args: Spanned<Vec<Spanned<Expr>>>,
    ) -> Option<(Value, Ty)> {
        let (callee, callee_span) = callee.into_pair();
        match callee {
            Expr::Error => panic_expr_error!(callee_span),
            Expr::Ident(ident) => {
                let mut arg_vals = Vec::<Value>::with_capacity(args.len());
                let Some(func_data) = self.cx.funcs.get(&ident) else {
                    self.err_reporter
                        .report(&Error::FuncDoesNotExist(ident).to_spanned(callee_span));
                    return None;
                };
                let sig = &func_data.sig;
                if args.len() != sig.args.len() {
                    self.err_reporter.report(
                        &Error::MismatchedArgCount(sig.args.len(), args.len())
                            .to_spanned(args.span()),
                    );
                }
                let sig = sig.clone(); // TODO: optimize this clone away.
                for (arg, (expect_ty, _)) in args.into_inner().into_iter().zip(&sig.args) {
                    let arg_span = arg.span();
                    let arg = self.build_expr(arg)?;
                    let (arg_val, _) = self.use_value(expect_ty.clone(), arg, arg_span)?;
                    arg_vals.push(arg_val);
                }
                match &sig.ret_ty.kind {
                    TyKind::Void => {
                        self.add_inst(MirInst::CallStatic(None, ident, arg_vals));
                        Some((Value::Void, Ty_::void()))
                    }
                    _ => {
                        let var_id = self.declare_var(sig.ret_ty.inner().clone(), None, true)?;
                        self.add_inst(MirInst::CallStatic(Some(var_id.into()), ident, arg_vals));
                        Some((Value::CopyPlace(var_id.into()), sig.ret_ty.into_inner()))
                    }
                }
            }
            _ => todo!("dynamic function calls"),
        }
    }
    fn build_expr(&mut self, expr: Spanned<Expr>) -> Option<(Value, Ty)> {
        let (expr, span) = expr.into_pair();
        match expr {
            Expr::Error => panic_expr_error!(span),
            Expr::Ident(..)
            | Expr::PrefixOp(PrefixOpKind::Deref, _)
            | Expr::Subscript(..)
            | Expr::FieldPath(..) => {
                let (place, ty) = self.build_place(expr.to_spanned(span))?;
                Some((Value::CopyPlace(place), ty))
            }
            Expr::NumLiteral(num) => {
                let (num_literal, ty) = self.build_num_literal(num, span)?;
                Some((Value::Num(num_literal), ty))
            }
            Expr::CharLiteral(c) => Some((
                Value::Char(c),
                TyKind::Int(Signness::Signed, IntSize::_8).to_ty(true, false, None),
            )),
            Expr::StrLiteral(_) => todo!(),
            Expr::PrefixOp(PrefixOpKind::UnarySub, expr) => {
                self.build_unary_sub(expr.into_unboxed())
            }
            Expr::PrefixOp(PrefixOpKind::UnaryAdd, expr) => {
                self.build_unary_add(expr.into_unboxed())
            }
            Expr::PrefixOp(PrefixOpKind::Ref, expr) => {
                let (place, ty) = self.build_place(expr.into_unboxed())?;
                let ty = TyKind::Ptr(Restrictness::NoRestrict, ty).to_ty(true, false, None);
                Some((Value::RefPlace(place), ty))
            }
            Expr::PrefixOp(_, _) => todo!(),
            Expr::PostfixOp(_, _) => todo!(),
            Expr::InfixOp(_, _, _) => todo!(),
            Expr::OpAssign(_, _, _) => todo!(),
            Expr::Typecast(_, _) => todo!(),
            Expr::Call(callee, args) => self.build_call(callee.into_unboxed(), args),
            Expr::List(_) => todo!(),
            Expr::Tenary(_, _, _) => todo!(),
            Expr::Return(..)
            | Expr::Labal(..)
            | Expr::Goto(..)
            | Expr::Break
            | Expr::Continue
            | Expr::Case(..)
            | Expr::Default
            | Expr::While(..)
            | Expr::DoWhile(..)
            | Expr::Switch(..)
            | Expr::For(..)
            | Expr::If(..)
            | Expr::Decl(..)
            | Expr::DeclList(..)
            | Expr::EmptyDecl(..) => {
                self.err_reporter.report(
                    &Error::ExprNotAllowed(ExprNotAllowedReason::StmtAsExpr).to_spanned(span),
                );
                None
            }
        }
    }
    fn build_assign(&mut self, lhs: Spanned<Expr>, rhs: Spanned<Expr>) -> Option<()> {
        let lhs_span = lhs.span();
        let (place, lhs_ty) = self.build_place(lhs)?;
        let place = place.to_spanned(lhs_span);
        let rhs_span = rhs.span();
        let rhs = self.build_expr(rhs)?;
        let (value, _) = self.use_value(lhs_ty, rhs, rhs_span)?;
        self.add_inst(MirInst::Assign(place.into_inner(), value));
        Some(())
    }
    pub fn build_stmt(&mut self, stmt: Spanned<Expr>) -> Option<()> {
        let (stmt, span) = stmt.into_pair();
        match stmt {
            Expr::Error => panic_expr_error!(span),
            Expr::Ident(_) => (),
            Expr::NumLiteral(_) => (),
            Expr::CharLiteral(_) => (),
            Expr::StrLiteral(_) => (),
            Expr::PrefixOp(_, _) => todo!(),
            Expr::PostfixOp(_, _) => todo!(),
            Expr::InfixOp(lhs, InfixOpKind::Eq, rhs) => {
                self.build_assign(lhs.into_unboxed(), rhs.into_unboxed())?;
            }
            Expr::InfixOp(_, _, _) => todo!(),
            Expr::OpAssign(_, _, _) => todo!(),
            Expr::Decl(item) => self.build_decl(item.to_spanned(span))?,
            Expr::DeclList(items) => {
                for item in items {
                    self.build_decl(item.to_spanned(span))?;
                }
            }
            Expr::EmptyDecl(_) => todo!(),
            Expr::Return(None) => {
                self.add_inst(MirInst::Term(MirTerm::Return(Value::Void)));
            }
            Expr::Return(Some(expr)) => {
                let value = self.build_expr(expr.into_unboxed())?;
                let expect_ty = self.mir_func.ret.clone();
                let (value, _) = self.use_value(expect_ty, value, span)?;
                self.add_inst(MirInst::Term(MirTerm::Return(value)));
            }
            Expr::Labal(_) => todo!(),
            Expr::Goto(_) => todo!(),
            Expr::Typecast(_, _) => todo!(),
            Expr::Call(callee, args) => _ = self.build_call(callee.into_unboxed(), args)?,
            Expr::Subscript(_, _) => todo!(),
            Expr::Break => todo!(),
            Expr::Continue => todo!(),
            Expr::Case(_) => todo!(),
            Expr::Default => todo!(),
            Expr::FieldPath(_, _) => todo!(),
            Expr::While(_, _) => todo!(),
            Expr::DoWhile(_, _) => todo!(),
            Expr::Switch(_, _) => todo!(),
            Expr::For(_, _, _, _) => todo!(),
            Expr::If(_, _) => todo!(),
            Expr::List(_) => todo!(),
            Expr::Tenary(_, _, _) => todo!(),
        }
        Some(())
    }

    pub fn build_place(&mut self, expr: Spanned<Expr>) -> Option<(Place, Ty)> {
        let (mut expr, span) = expr.into_pair();
        let mut projs = Vec::<PlaceProjection>::new();
        loop {
            match expr {
                Expr::Error => panic_expr_error!(span),
                Expr::Ident(name) => {
                    let Some(&var_id) = self.cx.locals.var(name) else {
                        self.err_reporter
                            .report(&Error::VarDoesNotExist(name).to_spanned(span));
                        return None;
                    };
                    let mut ty = self.mir_func.vars[var_id].ty.as_ref();
                    for proj in projs.iter().rev() {
                        match (proj, &ty.kind) {
                            (PlaceProjection::Deref, TyKind::Ptr(_, inner)) => {
                                ty = inner;
                            }
                            (PlaceProjection::Deref, _) => {
                                todo!()
                            }
                            (PlaceProjection::FieldDir(_), _) => todo!(),
                            (PlaceProjection::FieldInd(_), _) => todo!(),
                            (PlaceProjection::Index(_), _) => todo!(),
                        }
                    }
                    return Some((var_id.into(), Rc::new(ty.clone())));
                }
                Expr::PrefixOp(PrefixOpKind::Deref, inner) => {
                    projs.push(PlaceProjection::Deref);
                    expr = *inner.into_inner();
                }
                Expr::Subscript(_, _) => todo!(),
                Expr::FieldPath(_, _) => todo!(),
                _ => {
                    self.err_reporter
                        .report(&Error::ExprNoAssignable.to_spanned(span));
                    return None;
                }
            }
        }
    }
}
