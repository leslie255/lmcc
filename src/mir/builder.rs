use std::{borrow::Cow, collections::HashMap, rc::Rc};

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
    global: &'g GlobalContext,
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
                name: name,
            })
            .collect();
        let args: Vec<VarId> = vars.indices().collect();
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
    fn solve_typename(&self, ty: Ty) -> Cow<'g, Ty> {
        if let &TyKind::Typename(typename) = &ty.kind {
            let mut ty = self.global.names.typename(typename).unwrap();
            while let &TyKind::Typename(typename) = &ty.kind {
                ty = self.global.names.typename(typename).unwrap();
            }
            Cow::Borrowed(ty)
        } else {
            Cow::Owned(ty)
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
    fn build_expr(&mut self, expr: Spanned<Expr>) -> Option<(Value, Ty)> {
        let (expr, span) = expr.into_pair();
        match expr {
            Expr::Error => panic_expr_error!(),
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
                let lhs_ty = self.solve_typename(lhs_ty);
                let rhs_ty = self.solve_typename(rhs_ty);
                if lhs_ty != rhs_ty {
                    todo!("Auto type casting & type checking");
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
        todo!()
    }

    pub fn build_place(&mut self, expr: Spanned<Expr>) -> Option<(Place, Ty)> {
        let (expr, span) = expr.into_pair();
        match expr {
            Expr::Error => panic_expr_error!(),
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

/// Local context inside a function.
struct LocalContext {
    var_ids: Vec<HashMap<IdentStr, VarId>>,
}
impl LocalContext {
    fn enters_block(&mut self) {
        self.var_ids.push(HashMap::new());
    }
    fn leaves_block(&mut self) {
        self.var_ids.pop().unwrap();
    }
    /// Returns `Err` if variable of the same name already exist.
    fn add_variable(&mut self, name: IdentStr, id: VarId) -> Result<(), ()> {
        let prev = self.var_ids.last_mut().unwrap().insert(name, id);
        if prev.is_some() {
            Err(())
        } else {
            Ok(())
        }
    }
    fn var_id(&self, name: IdentStr) -> Option<VarId> {
        self.var_ids
            .iter()
            .rev()
            .find_map(|vars| vars.get(&name).copied())
    }
}
