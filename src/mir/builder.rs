use std::rc::Rc;

use index_vec::{index_vec, IndexVec};

use crate::{
    ast::{
        AssignOpKind, DeclItem, Expr, FloatSize, InfixOpKind, IntSize, PrefixOpKind, Restrictness,
        Signature, Signness, Ty, TyKind, Ty_, VarSpecifier,
    },
    consteval::consteval_ast,
    error::{Error, ErrorReporter, ExprNotAllowedReason, Span, Spanned, ToSpanned},
    mir::{BinOpKind, BsKind, NumLiteralContent},
    token::NumValue,
    utils::IdentStr,
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
                    func_builder.build_stmt(stmt.as_ref());
                }
                cx.funcs.get_mut(&name).unwrap().mir_func = Some(func_builder.mir_func);
                cx.locals.leaves_block();
            }
        }
        _ => todo!(),
    }
}

/// Find the result type of when types are operated together.
pub fn merge_tys(lhs_ty: &Ty_, rhs_ty: &Ty_) -> Option<Ty> {
    match (&lhs_ty.kind, &rhs_ty.kind) {
        (
            _,
            &TyKind::FixedArr(..)
            | TyKind::Struct(..)
            | TyKind::Union(..)
            | TyKind::Enum(..)
            | TyKind::Void,
        )
        | (
            &TyKind::FixedArr(..)
            | TyKind::Struct(..)
            | TyKind::Union(..)
            | TyKind::Enum(..)
            | TyKind::Void,
            _,
        ) => None,
        (_, TyKind::Error) => panic!("reached internal `ERROR` type in MIR stage"),
        (TyKind::Error, _) => panic!("reached internal `ERROR` type in MIR stage"),
        (&TyKind::Int(lhs_sign, lhs_size), &TyKind::Int(rhs_sign, rhs_size)) => {
            let sign = if lhs_sign.is_unsigned() || rhs_sign.is_unsigned() {
                Signness::Unsigned
            } else {
                Signness::Signed
            };
            let size = if lhs_size == rhs_size {
                lhs_size
            } else {
                lhs_size.min(rhs_size).min(IntSize::_32)
            };
            Some(TyKind::Int(sign, size).to_ty(false, false, None))
        }
        (&TyKind::Float(lhs_size), &TyKind::Float(rhs_size)) => {
            Some(TyKind::Float(lhs_size.min(rhs_size)).to_ty(false, false, None))
        }
        (TyKind::Int(..), &TyKind::Float(size)) | (&TyKind::Float(size), TyKind::Int(..)) => {
            Some(TyKind::Float(size).to_ty(false, false, None))
        }
        (&TyKind::Int(sign, size), TyKind::Bool) | (TyKind::Bool, &TyKind::Int(sign, size)) => {
            Some(TyKind::Int(sign, size.min(IntSize::_32)).to_ty(false, false, None))
        }
        (&TyKind::Int(..), TyKind::Ptr(_, inner))
        | (TyKind::Ptr(_, inner), &TyKind::Int(..))
        | (TyKind::Bool, TyKind::Ptr(_, inner))
        | (TyKind::Ptr(_, inner), TyKind::Bool) => {
            Some(TyKind::Ptr(Restrictness::NoRestrict, inner.clone()).to_ty(false, false, None))
        }
        (&TyKind::Float(size), TyKind::Bool) | (TyKind::Bool, &TyKind::Float(size)) => {
            Some(TyKind::Float(size).to_ty(false, false, None))
        }
        (TyKind::Float(..), TyKind::Ptr(..)) | (TyKind::Ptr(..), TyKind::Float(..)) => None,
        (TyKind::Bool, TyKind::Bool) => {
            Some(TyKind::Int(Signness::Signed, IntSize::_32).to_ty(false, false, None))
        }
        (TyKind::Ptr(..), TyKind::Ptr(..)) => None,
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
    fn use_num(
        &mut self,
        expect_ty: Ty,
        (num, num_ty): (NumLiteralContent, Ty),
        span: Span,
    ) -> Option<Value> {
        match &expect_ty.kind {
            TyKind::Int(..) => {
                let num = match num {
                    NumLiteralContent::U(..) | NumLiteralContent::I(..) => {
                        Value::Num(num, expect_ty.clone())
                    }
                    NumLiteralContent::F(f) if f.is_sign_positive() => {
                        let u = f.round() as u64;
                        Value::Num(u.into(), expect_ty.clone())
                    }
                    NumLiteralContent::F(f) if f.is_sign_negative() => {
                        let u = f.round() as u64;
                        Value::Num(u.into(), expect_ty.clone())
                    }
                    _ => unreachable!(),
                };
                Some(num)
            }
            TyKind::Bool => {
                let is_true = match num {
                    NumLiteralContent::U(u) => u != 0,
                    NumLiteralContent::I(i) => i != 0,
                    NumLiteralContent::F(f) => f != 0.0,
                };
                let num: u64 = if is_true { 1 } else { 0 };
                Some(Value::Num(num.into(), expect_ty.clone()))
            }
            TyKind::Float(..) => {
                let num = match num {
                    NumLiteralContent::U(u) => Value::Num((u as f64).into(), expect_ty.clone()),
                    NumLiteralContent::I(i) => Value::Num((i as f64).into(), expect_ty.clone()),
                    f @ NumLiteralContent::F(..) => Value::Num(f, expect_ty.clone()),
                };
                Some(num)
            }
            TyKind::Ptr(..) => match num {
                NumLiteralContent::U(..) | NumLiteralContent::I(..) => {
                    Some(Value::Num(num, expect_ty.clone()))
                }
                _ => {
                    self.err_reporter
                        .report(&Error::MismatchedType(&expect_ty, &num_ty).to_spanned(span));
                    None
                }
            },
            _ => {
                self.err_reporter
                    .report(&Error::MismatchedType(&expect_ty, &num_ty).to_spanned(span));
                None
            }
        }
    }
    /// If types are unequal, elaborates typecast or report error.
    /// If types are equal, return the original value.
    fn use_value(
        &mut self,
        expect_ty: Ty,
        (value, value_ty): (Value, Ty),
        span: Span,
    ) -> Option<Value> {
        if value_ty.kind_eq(&expect_ty) {
            return Some(value);
        }
        if value_ty.is_arr() || expect_ty.is_arr() {
            todo!("arrays");
        }
        // Compile-time implicit cast for numeric types.
        if let &Value::Num(num, _) = &value {
            return self.use_num(expect_ty, (num, value_ty), span);
        }
        // We already checked earlier that expect_ty does not equal to value_ty.
        if expect_ty.is_arithmatic_or_ptr() && value_ty.is_arithmatic_or_ptr() {
            let var_id = self.declare_var(expect_ty.clone(), None, true)?;
            self.add_inst(MirInst::Tycast(
                var_id.into(),
                expect_ty.clone(),
                value_ty,
                value,
            ));
            Some(Value::CopyPlace(var_id.into()))
        } else {
            self.err_reporter
                .report(&Error::MismatchedType(&expect_ty, &value_ty).to_spanned(span));
            None
        }
    }
    fn build_num_literal(&self, num: NumValue, span: Span) -> Option<(NumLiteralContent, Ty)> {
        match num {
            NumValue::I(i) => {
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
            NumValue::F(f) => Some((
                NumLiteralContent::F(f),
                TyKind::Float(FloatSize::_64).to_ty(true, false, None),
            )),
        }
    }
    fn build_unary_add(&mut self, operand: Spanned<&Expr>) -> Option<(Value, Ty)> {
        let span = operand.span();
        let (value, ty) = self.build_expr(operand)?;
        if !matches!(&ty.kind, TyKind::Int(..) | TyKind::Float(..) | TyKind::Bool) {
            self.err_reporter
                .report(&Error::NonNumericInUnary.to_spanned(span));
            return None;
        }
        Some((value, ty))
    }
    fn build_unary_sub(&mut self, operand: Spanned<&Expr>) -> Option<(Value, Ty)> {
        let (operand, span) = operand.into_pair();
        match operand {
            Expr::Error => panic_expr_error!(span),
            &Expr::NumLiteral(num) => {
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
                Some((Value::Num(num, ty.clone()), ty))
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
    fn build_decl(&mut self, item: Spanned<&DeclItem>) -> Option<()> {
        let (item, span) = item.into_pair();
        match item {
            DeclItem::Var(VarSpecifier::Typedef, _, _, None) => Some(()),
            DeclItem::Var(VarSpecifier::Typedef, _, _, Some(_rhs)) => {
                self.err_reporter
                    .report(&Error::TypedefWithRhs.to_spanned(span));
                None
            }
            // `register` storage specifier is ignored.
            &DeclItem::Var(
                VarSpecifier::Auto | VarSpecifier::Register | VarSpecifier::Unspecified,
                ref ty,
                name,
                ref rhs,
            ) => {
                if let Some(rhs) = rhs {
                    // Have to manually re-write part of `build_assign` here because partial borrow.
                    let rhs_span = rhs.span();
                    let rhs = self.build_expr(rhs.as_deref())?;
                    let lhs_ty = ty.inner().clone();
                    if lhs_ty.is_void() {
                        self.err_reporter
                            .report(&Error::IllegalVoidTy.to_spanned(span));
                        return None;
                    }
                    let value = self.use_value(lhs_ty.clone(), rhs, rhs_span)?;
                    let var_id = self.declare_var(lhs_ty, Some(name), false)?;
                    let place = Place::from(var_id).to_spanned(name.span());
                    self.add_inst(MirInst::Assign(place.into_inner(), value));
                } else {
                    self.declare_var(ty.inner().clone(), Some(name), false)?;
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
        callee: Spanned<&Expr>,
        args: Spanned<&[Spanned<Expr>]>,
    ) -> Option<(Value, Ty)> {
        let (callee, callee_span) = callee.into_pair();
        match callee {
            Expr::Error => panic_expr_error!(callee_span),
            &Expr::Ident(ident) => {
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
                    let arg = self.build_expr(arg.as_ref())?;
                    let arg_val = self.use_value(expect_ty.clone(), arg, arg_span)?;
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
    fn build_expr(&mut self, expr: Spanned<&Expr>) -> Option<(Value, Ty)> {
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
            &Expr::NumLiteral(num) => {
                let (num_literal, ty) = self.build_num_literal(num, span)?;
                Some((Value::Num(num_literal, ty.clone()), ty))
            }
            &Expr::CharLiteral(c) => Some((
                Value::Char(c),
                TyKind::Int(Signness::Signed, IntSize::_8).to_ty(true, false, None),
            )),
            Expr::StrLiteral(_) => todo!(),
            Expr::PrefixOp(PrefixOpKind::UnarySub, expr) => self.build_unary_sub(expr.as_deref()),
            Expr::PrefixOp(PrefixOpKind::UnaryAdd, expr) => self.build_unary_add(expr.as_deref()),
            Expr::PrefixOp(PrefixOpKind::Ref, expr) => {
                let (place, ty) = self.build_place(expr.as_deref())?;
                let ty = TyKind::Ptr(Restrictness::NoRestrict, ty).to_ty(true, false, None);
                Some((Value::RefPlace(place), ty))
            }
            Expr::PrefixOp(_, _) => todo!(),
            Expr::PostfixOp(_, _) => todo!(),
            Expr::InfixOp(_, InfixOpKind::Assign, _) => todo!(),
            Expr::InfixOp(lhs, op @ InfixOpKind::Bsl | op @ InfixOpKind::Bsr, rhs) => {
                let op = match op {
                    InfixOpKind::Bsl => BsKind::Bsl,
                    InfixOpKind::Bsr => BsKind::Bsr,
                    _ => unreachable!(),
                };
                let Some(NumValue::I(rhs)) = consteval_ast(rhs.as_deref()) else {
                    self.err_reporter
                        .report(&Error::InvalidBsOper.to_spanned(span));
                    return None;
                };
                let Ok(rhs) = u64::try_from(rhs) else {
                    self.err_reporter
                        .report(&Error::InvalidBsOper.to_spanned(span));
                    return None;
                };
                let (lhs, ty) = self.build_expr(lhs.as_deref())?;
                if !ty.is_arithmatic_or_ptr() {
                    self.err_reporter
                        .report(&Error::InvalidTyForOp(&ty).to_spanned(span));
                    return None;
                }
                let var_id = self.declare_var(ty.clone(), None, true)?;
                self.add_inst(MirInst::Bs(var_id.into(), lhs, op, rhs));
                Some((Value::CopyPlace(var_id.into()), ty))
            }
            Expr::InfixOp(expr0, InfixOpKind::Comma, expr1) => {
                self.build_expr(expr0.as_deref());
                self.build_expr(expr1.as_deref())
            }
            Expr::InfixOp(lhs, op, rhs) => {
                let lhs_span = lhs.span();
                let rhs_span = rhs.span();
                let (op, is_cmp) = match op {
                    InfixOpKind::Add => (BinOpKind::Add, false),
                    InfixOpKind::Sub => (BinOpKind::Sub, false),
                    InfixOpKind::Mul => (BinOpKind::Mul, false),
                    InfixOpKind::Div => (BinOpKind::Div, false),
                    InfixOpKind::Rem => (BinOpKind::Rem, false),
                    InfixOpKind::BitAnd => (BinOpKind::BitAnd, false),
                    InfixOpKind::BitOr => (BinOpKind::BitOr, false),
                    InfixOpKind::BitXor => (BinOpKind::BitXor, false),
                    InfixOpKind::And => (BinOpKind::And, false),
                    InfixOpKind::Or => (BinOpKind::Or, false),
                    InfixOpKind::Gt => (BinOpKind::Gt, true),
                    InfixOpKind::Ge => (BinOpKind::Ge, true),
                    InfixOpKind::Lt => (BinOpKind::Lt, true),
                    InfixOpKind::Le => (BinOpKind::Le, true),
                    InfixOpKind::Eq => (BinOpKind::Eq, true),
                    InfixOpKind::Ne => (BinOpKind::Ne, true),
                    InfixOpKind::Assign
                    | InfixOpKind::Comma
                    | InfixOpKind::Bsl
                    | InfixOpKind::Bsr => unreachable!(),
                };
                let (lhs_val, lhs_ty) = self.build_expr(lhs.as_deref())?;
                let (rhs_val, rhs_ty) = self.build_expr(rhs.as_deref())?;
                let Some(mut result_ty) = merge_tys(&lhs_ty, &rhs_ty) else {
                    self.err_reporter
                        .report(&Error::IncompatibleTyForBinOp(&lhs_ty, &rhs_ty).to_spanned(span));
                    return None;
                };
                if is_cmp {
                    result_ty = TyKind::Bool.to_ty(false, false, None);
                }
                let lhs_val = self.use_value(result_ty.clone(), (lhs_val, lhs_ty), lhs_span)?;
                let rhs_val = self.use_value(result_ty.clone(), (rhs_val, rhs_ty), rhs_span)?;
                let var_id = self.declare_var(result_ty.clone(), None, true)?;
                self.add_inst(MirInst::BinOp(var_id.into(), lhs_val, op, rhs_val));
                Some((Value::CopyPlace(var_id.into()), result_ty))
            }
            Expr::Typecast(target_ty, expr) => {
                self.build_typecast(target_ty.clone(), expr.as_deref())
            }
            Expr::Call(callee, args) => self.build_call(callee.as_deref(), args.as_deref()),
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
            | Expr::EmptyDecl(..)
            | Expr::OpAssign(..) => {
                self.err_reporter.report(
                    &Error::ExprNotAllowed(ExprNotAllowedReason::StmtAsExpr).to_spanned(span),
                );
                None
            }
        }
    }
    fn build_assign(&mut self, lhs: Spanned<&Expr>, rhs: Spanned<&Expr>) -> Option<()> {
        let lhs_span = lhs.span();
        let (place, lhs_ty) = self.build_place(lhs)?;
        if lhs_ty.is_const {
            self.err_reporter
                .report(&Error::ExprNotAssignable.to_spanned(lhs_span));
            self.build_expr(rhs)?;
            return None;
        }
        let rhs_span = rhs.span();
        let rhs = self.build_expr(rhs)?;
        let value = self.use_value(lhs_ty, rhs, rhs_span)?;
        self.add_inst(MirInst::Assign(place, value));
        Some(())
    }
    pub fn build_typecast(&mut self, target_ty: Ty, expr: Spanned<&Expr>) -> Option<(Value, Ty)> {
        let expr_span = expr.span();
        let (value, source_ty) = self.build_expr(expr)?;
        let var_id = self.declare_var(target_ty.clone(), None, true)?;
        if !source_ty.is_arithmatic_or_ptr() || !target_ty.is_arithmatic_or_ptr() {
            self.err_reporter
                .report(&Error::InvalidTypecast(&source_ty, &target_ty).to_spanned(expr_span));
            return None;
        }
        self.add_inst(MirInst::Tycast(
            var_id.into(),
            source_ty,
            target_ty.clone(),
            value,
        ));
        Some((Value::CopyPlace(var_id.into()), target_ty))
    }
    pub fn build_stmt(&mut self, stmt: Spanned<&Expr>) -> Option<()> {
        let (stmt, span) = stmt.into_pair();
        match stmt {
            Expr::Error => panic_expr_error!(span),
            Expr::Ident(_) => (),
            Expr::NumLiteral(_) => (),
            Expr::CharLiteral(_) => (),
            Expr::StrLiteral(_) => (),
            Expr::PrefixOp(_, _) => todo!(),
            Expr::PostfixOp(_, _) => todo!(),
            Expr::InfixOp(lhs, InfixOpKind::Assign, rhs) => {
                self.build_assign(lhs.as_deref(), rhs.as_deref())?;
            }
            Expr::InfixOp(expr0, InfixOpKind::Comma, expr1) => {
                if self.build_expr(expr0.as_deref()).is_none()
                    || self.build_expr(expr1.as_deref()).is_none()
                {
                    return None;
                }
            }
            Expr::InfixOp(lhs, _, rhs) => {
                let (_, lhs_ty) = self.build_expr(lhs.as_deref())?;
                let (_, rhs_ty) = self.build_expr(rhs.as_deref())?;
                if merge_tys(&lhs_ty, &rhs_ty).is_none() {
                    self.err_reporter
                        .report(&Error::IncompatibleTyForBinOp(&lhs_ty, &rhs_ty).to_spanned(span));
                    return None;
                }
            }
            Expr::OpAssign(_, AssignOpKind::Bsl | AssignOpKind::Bsr, _) => todo!(),
            Expr::OpAssign(lhs, op, rhs) => {
                let lhs_span = lhs.span();
                let rhs_span = rhs.span();
                let op = match op {
                    AssignOpKind::Add => BinOpKind::Add,
                    AssignOpKind::Sub => BinOpKind::Sub,
                    AssignOpKind::Mul => BinOpKind::Mul,
                    AssignOpKind::Div => BinOpKind::Div,
                    AssignOpKind::Rem => BinOpKind::Rem,
                    AssignOpKind::BitAnd => BinOpKind::BitAnd,
                    AssignOpKind::BitOr => BinOpKind::BitOr,
                    AssignOpKind::BitXor => BinOpKind::BitXor,
                    AssignOpKind::Bsl | AssignOpKind::Bsr => unreachable!(),
                };
                let (lhs_val, lhs_ty) = self.build_expr(lhs.as_deref())?;
                let (rhs_val, rhs_ty) = self.build_expr(rhs.as_deref())?;
                let Some(result_ty) = merge_tys(&lhs_ty, &rhs_ty) else {
                    self.err_reporter
                        .report(&Error::IncompatibleTyForBinOp(&lhs_ty, &rhs_ty).to_spanned(span));
                    return None;
                };
                let lhs_val = self.use_value(result_ty.clone(), (lhs_val, lhs_ty), lhs_span)?;
                let rhs_val = self.use_value(result_ty.clone(), (rhs_val, rhs_ty), rhs_span)?;
                let result_var_id = self.declare_var(result_ty.clone(), None, true)?;
                self.add_inst(MirInst::BinOp(result_var_id.into(), lhs_val, op, rhs_val));
                let lhs = lhs.as_deref();
                let (place, lhs_ty) = self.build_place(lhs)?;
                if lhs_ty.is_const {
                    self.err_reporter
                        .report(&Error::ExprNotAssignable.to_spanned(lhs_span));
                    return None;
                }
                self.add_inst(MirInst::Assign(
                    place,
                    Value::CopyPlace(result_var_id.into()),
                ));
            }
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
                let value = self.build_expr(expr.as_deref())?;
                let expect_ty = self.mir_func.ret.clone();
                let value = self.use_value(expect_ty, value, span)?;
                self.add_inst(MirInst::Term(MirTerm::Return(value)));
            }
            Expr::Labal(_) => todo!(),
            Expr::Goto(_) => todo!(),
            // typecast itself have no side effect.
            Expr::Typecast(_, expr) => _ = self.build_expr(expr.as_deref())?,
            Expr::Call(callee, args) => _ = self.build_call(callee.as_deref(), args.as_deref())?,
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

    pub fn build_place(&mut self, expr: Spanned<&Expr>) -> Option<(Place, Ty)> {
        let (mut expr, span) = expr.into_pair();
        let mut projs = Vec::<PlaceProjection>::new();
        loop {
            match expr {
                Expr::Error => panic_expr_error!(span),
                &Expr::Ident(name) => {
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
                    expr = inner.as_deref().into_inner();
                }
                Expr::Subscript(_, _) => todo!(),
                Expr::FieldPath(_, _) => todo!(),
                _ => {
                    self.err_reporter
                        .report(&Error::ExprNotAssignable.to_spanned(span));
                    return None;
                }
            }
        }
    }
}
