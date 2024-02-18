use std::rc::Rc;

use index_vec::{index_vec, IndexVec};

use crate::{
    ast::{
        DeclItem, Expr, FloatSize, InfixOpKind, IntSize, PrefixOpKind, Signature, Signness, Ty,
        TyKind,
    },
    error::{Error, ErrorReporter, Span, Spanned, ToSpanned},
    mir::NumLiteralContent,
    token::NumValue,
    utils::IdentStr,
};

use super::{
    BlockId, FuncData, GlobalContext, MirBlock, MirFunction, MirInst, Place, Value, VarId, VarInfo,
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

pub fn make_mir_for_item(
    global: &mut GlobalContext,
    item: DeclItem,
    err_reporter: Rc<ErrorReporter>,
) {
    match item {
        DeclItem::Func(_func_speci, ref sig, name, body) => {
            global.funcs.insert(
                name,
                FuncData {
                    sig: sig.clone(),
                    mir_func: None,
                },
            );
            if let Some(body) = body {
                global.names.enters_block();
                let mut func_builder = MirFuncBuilder::new(global, err_reporter, name, sig.clone());
                for stmt in body {
                    func_builder.build_stmt(stmt);
                }
                global.funcs.get_mut(&name).unwrap().mir_func = Some(func_builder.mir_func);
                global.names.leaves_block();
            }
        }
        _ => todo!(),
    }
}

struct MirFuncBuilder<'g> {
    global: &'g mut GlobalContext,
    err_reporter: Rc<ErrorReporter>,
    mir_func: MirFunction,
    /// The "cursor" for writing new instructions onto.
    current_block: BlockId,
    func_name: IdentStr,
}

impl<'g> MirFuncBuilder<'g> {
    /// Function itself must have already been added to global context.
    /// Requires calling `GlobalContext::{enters_block, leaves_block}` outside.
    pub fn new(
        global: &'g mut GlobalContext,
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
            })
            .collect();
        let args: Vec<VarId> = vars.indices().collect();
        for (id, info) in vars.iter_enumerated() {
            if let Some(name) = info.name {
                _ = global
                    .names
                    .add_var(*name, (id, info.ty.clone()))
                    .inspect_err(|()| {
                        err_reporter
                            .report(&Error::RedefinitionOfVar(*name).to_spanned(name.span()))
                    });
            }
        }
        let mir_func = MirFunction {
            args,
            ret: sig.ret_ty.into_inner(),
            vars,
            blocks: index_vec![MirBlock::default()],
        };
        global.names.enters_block();
        Self {
            global,
            err_reporter,
            mir_func,
            current_block: BlockId(0),
            func_name,
        }
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
    fn solve_typename<'a: 'g, 'b: 'g>(&'a self, ty: &'b Ty) -> &'g Ty {
        if let &TyKind::Typename(typename) = &ty.kind {
            let mut ty = self.global.names.typename(typename).unwrap();
            while let &TyKind::Typename(typename) = &ty.kind {
                ty = self.global.names.typename(typename).unwrap();
            }
            ty
        } else {
            ty
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
                        TyKind::Int(Signness::Unsigned, size).to_ty(true, false),
                    ))
                } else if let Ok(i) = i64::try_from(i) {
                    let size = if i32::try_from(i).is_ok() {
                        IntSize::_32
                    } else {
                        IntSize::_64
                    };
                    Some((
                        NumLiteralContent::I(i),
                        TyKind::Int(Signness::Signed, size).to_ty(true, false),
                    ))
                } else {
                    self.err_reporter
                        .report(&Error::OutOfRangeNumber(i).to_spanned(span));
                    None
                }
            }
            crate::token::NumValue::F(f) => Some((
                NumLiteralContent::F(f),
                TyKind::Float(FloatSize::_64).to_ty(true, false),
            )),
        }
    }
    fn build_unary_add(&mut self, operand: Spanned<Expr>) -> Option<(Value, Ty)> {
        let span = operand.span();
        let (value, ty) = self.build_expr(operand)?;
        match &self.solve_typename(&ty).kind {
            TyKind::Int(_, _) | TyKind::Float(_) | TyKind::Bool => (),
            TyKind::Typename(_) => unreachable!(),
            _ => {
                self.err_reporter
                    .report(&Error::NonNumericInUnary.to_spanned(span));
                return None;
            }
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
                        ty = match &ty.kind {
                            &TyKind::Int(_, size) => TyKind::Int(Signness::Signed, size).to_ty(true, false),
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
                match &self.solve_typename(&ty).kind {
                    TyKind::Int(_, _) | TyKind::Float(_) | TyKind::Bool => (),
                    TyKind::Typename(_) => unreachable!(),
                    _ => {
                        self.err_reporter
                            .report(&Error::NonNumericInUnary.to_spanned(span));
                        return None;
                    }
                }
                todo!()
            }
        }
    }
    fn build_expr(&mut self, expr: Spanned<Expr>) -> Option<(Value, Ty)> {
        let (expr, span) = expr.into_pair();
        match expr {
            Expr::Error => panic_expr_error!(span),
            Expr::Ident(..) | Expr::PrefixOp(PrefixOpKind::Deref, _) => {
                let (place, ty) = self.build_place(expr.to_spanned(span))?;
                Some((Value::CopyPlace(place), ty))
            }
            Expr::NumLiteral(num) => {
                let (num_literal, ty) = self.build_num_literal(num, span)?;
                Some((Value::Num(num_literal), ty))
            }
            Expr::CharLiteral(c) => Some((
                Value::Char(c),
                TyKind::Int(Signness::Signed, IntSize::_8).to_ty(true, false),
            )),
            Expr::StrLiteral(_) => todo!(),
            Expr::PrefixOp(PrefixOpKind::UnarySub, expr) => {
                self.build_unary_sub(expr.into_unboxed())
            }
            Expr::PrefixOp(PrefixOpKind::UnaryAdd, expr) => {
                self.build_unary_add(expr.into_unboxed())
            }
            Expr::PrefixOp(_, _) => todo!(),
            Expr::PostfixOp(_, _) => todo!(),
            Expr::InfixOp(_, _, _) => todo!(),
            Expr::OpAssign(_, _, _) => todo!(),
            Expr::Typecast(_, _) => todo!(),
            Expr::Call(_, _) => todo!(),
            Expr::Subscript(_, _) => todo!(),
            Expr::FieldPath(_, _) => todo!(),
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
                self.err_reporter
                    .report(&Error::ExprNotAllowed.to_spanned(span));
                None
            }
        }
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
                let (place, lhs_ty) = self.build_place(lhs.into_unboxed())?;
                let (value, rhs_ty) = self.build_expr(rhs.into_unboxed())?;
                let lhs_ty = self.solve_typename(&lhs_ty);
                let rhs_ty = self.solve_typename(&rhs_ty);
                if lhs_ty.kind != rhs_ty.kind {
                    todo!("Auto type casting & type checking (lhs: {lhs_ty:?}, rhs: {rhs_ty:?})");
                }
                self.add_inst(MirInst::Assign(place, value));
            }
            Expr::InfixOp(_, _, _) => todo!(),
            Expr::OpAssign(_, _, _) => todo!(),
            Expr::Decl(_) => todo!(),
            Expr::DeclList(_) => todo!(),
            Expr::EmptyDecl(_) => todo!(),
            Expr::Return(_) => todo!(),
            Expr::Labal(_) => todo!(),
            Expr::Goto(_) => todo!(),
            Expr::Typecast(_, _) => todo!(),
            Expr::Call(_, _) => todo!(),
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
        let (expr, span) = expr.into_pair();
        match expr {
            Expr::Error => panic_expr_error!(span),
            Expr::Ident(name) => {
                let Some(&(var_id, ref ty)) = self.global.names.var(name) else {
                    self.err_reporter
                        .report(&Error::VarDoesNotExist(name).to_spanned(span));
                    return None;
                };
                Some((
                    Place {
                        root: var_id,
                        projections: Vec::new(),
                    },
                    ty.clone(),
                ))
            }
            Expr::PrefixOp(PrefixOpKind::Deref, _) => todo!(),
            Expr::Subscript(_, _) => todo!(),
            Expr::FieldPath(_, _) => todo!(),
            _ => {
                self.err_reporter
                    .report(&Error::ExprNoAssignable.to_spanned(span));
                None
            }
        }
    }
}
