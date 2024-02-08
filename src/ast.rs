#![allow(dead_code)]
use std::{
    fmt::{self, Debug},
    rc::Rc,
};

use crate::{error::Spanned, token::NumValue, utils::IdentStr};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// For propagating erors forward.
    Error,
    Ident(IdentStr),
    NumLiteral(NumValue),
    CharLiteral(u8),
    StrLiteral(Rc<[u8]>),
    PrefixOp(PrefixOpKind, Box<Spanned<Expr>>),
    PostfixOp(Box<Spanned<Expr>>, PostfixOpKind),
    /// Not including assignments or memeber accessing because the LHS/RHS for those aren't really expressions.
    /// Does include BSL/BSR, compile error later down the line if the RHS is not an integer literal.
    /// Note that the `a * b;` situation is treated as a mul operation in AST stage.
    InfixOp(Box<Spanned<Expr>>, InfixOpKind, Box<Spanned<Expr>>),
    InfixOpAssign(Box<Spanned<Expr>>, AssignOpKind, Box<Spanned<Expr>>),
    Assign(Box<Spanned<Expr>>, Box<Spanned<Expr>>),
    VarDecl(VarSpecifier, Spanned<Ty>, IdentStr),
    VarDeclInit(VarSpecifier, Spanned<Ty>, IdentStr, Box<Spanned<Expr>>),
    DeclList(Vec<DeclItem>),
    Return(Option<Box<Spanned<Expr>>>),
    FuncDef(
        FuncSpecifier,
        IdentStr,
        Signature,
        Option<Vec<Spanned<Expr>>>,
    ),
    Labal(IdentStr),
    Goto(IdentStr),
    Break,
    Continue,
}
impl Expr {
    pub fn omits_semicolon(&self) -> bool {
        match self {
            Expr::Error | Expr::FuncDef(..) | Expr::Labal(..) => true,
            _ => false,
        }
    }
}
#[derive(Clone, PartialEq, Eq)]
pub enum TyKind {
    Int(Signness, IntSize),
    Float(FloatSize),
    Bool,
    Ptr(Restrictness, Box<Ty>),
    FixedArr(Box<Ty>, u64),
    Struct(IdentStr),
    Union(IdentStr),
    Enum(IdentStr),
    Typename(IdentStr),
    Void,
    Error,
}
impl TyKind {
    pub fn to_ty(self, is_const: bool, is_volatile: bool) -> Ty {
        Ty {
            kind: self,
            is_const,
            is_volatile,
        }
    }
}
#[derive(Clone, PartialEq, Eq)]
pub struct Ty {
    pub is_const: bool,
    pub is_volatile: bool,
    pub kind: TyKind,
}
impl Ty {
    pub fn is_arr(&self) -> bool {
        matches!(self.kind, TyKind::FixedArr(..))
    }
    pub fn decayed(self) -> Self {
        match self.kind {
            TyKind::FixedArr(ty, _) => Self {
                kind: TyKind::Ptr(Restrictness::NoRestrict, ty),
                is_const: self.is_const,
                is_volatile: self.is_volatile,
            },
            _ => self,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Restrictness {
    Restrict,
    NoRestrict,
}
impl Restrictness {
    pub const fn is_restrict(self) -> bool {
        matches!(self, Self::Restrict)
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Signness {
    Signed,
    Unsigned,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntSize {
    /// char
    _8,
    /// short
    _16,
    /// int
    _32,
    /// long / long long
    _64,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FloatSize {
    /// float
    _32,
    /// double
    _64,
    /// Unsupported but defined here for error reporting.
    LongDouble,
}
/// Formally known as **storage specifiers**.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarSpecifier {
    /// `Unspecified` is for when the user did not add a storage specifier.
    /// `Auto` is for when the user specified `auto`.
    /// This is useful for error reporting when the user mistakes `auto` for `auto` in C++/C23.
    Unspecified,
    Static,
    Register,
    Extern,
    /// `Unspecified` is for when the user did not add a storage specifier.
    /// `Auto` is for when the user specified `auto`.
    /// This is useful for error reporting when the user mistakes `auto` for `auto` in C++/C23.
    Auto,
    Typedef,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkageSpecifier {
    Unspecified,
    Static,
    Extern,
}
/// Keywords that appears before a function declaration.
/// Unlike the term used in documents of C standards (where **function specifier**  `inline`), function specifiers here means `inline` and **linkage specifier**.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FuncSpecifier {
    pub inline: bool,
    pub linkage: LinkageSpecifier,
}
/// Not including sizeof because the RHS isn't an expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PrefixOpKind {
    BitNot,
    Not,
    UnaryAdd,
    UnarySub,
    PreInc,
    PreDec,
    /// Formally the Address-of operator
    Ref,
    /// Formally the Indirection operator
    Deref,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PostfixOpKind {
    PostInc,
    PostDec,
}
/// Not including assignments or memeber accessing because the LHS/RHS for those aren't really expressions.
/// Does include BSL/BSR, compile error later down the line if the RHS is not an integer literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InfixOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    And,
    Or,
    Xor,
    Gr,
    GrEq,
    Le,
    LeEq,
    Eq,
    Ne,
    Comma,
    Bsl,
    Bsr,
}
impl From<AssignOpKind> for InfixOpKind {
    fn from(value: AssignOpKind) -> Self {
        value.to_infix_op_kind()
    }
}
/// Operators that can be used for assign-with-operator (+=, -=, etc.).
/// It is a subset of `InfixOpKind`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssignOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    BitAnd,
    BitOr,
    BitXor,
    Gr,
    GrEq,
    Le,
    LeEq,
    Eq,
    Ne,
    Comma,
    Bsl,
    Bsr,
}
impl AssignOpKind {
    pub const fn to_infix_op_kind(self) -> InfixOpKind {
        match self {
            AssignOpKind::Add => InfixOpKind::Add,
            AssignOpKind::Sub => InfixOpKind::Sub,
            AssignOpKind::Mul => InfixOpKind::Mul,
            AssignOpKind::Div => InfixOpKind::Div,
            AssignOpKind::Mod => InfixOpKind::Mod,
            AssignOpKind::BitAnd => InfixOpKind::BitAnd,
            AssignOpKind::BitOr => InfixOpKind::BitOr,
            AssignOpKind::BitXor => InfixOpKind::BitXor,
            AssignOpKind::Gr => InfixOpKind::Gr,
            AssignOpKind::GrEq => InfixOpKind::GrEq,
            AssignOpKind::Le => InfixOpKind::Le,
            AssignOpKind::LeEq => InfixOpKind::LeEq,
            AssignOpKind::Eq => InfixOpKind::Eq,
            AssignOpKind::Ne => InfixOpKind::Ne,
            AssignOpKind::Comma => InfixOpKind::Comma,
            AssignOpKind::Bsl => InfixOpKind::Bsl,
            AssignOpKind::Bsr => InfixOpKind::Bsr,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Signature {
    pub ret_ty: Spanned<Ty>,
    pub args: Vec<(Ty, Option<IdentStr>)>,
}

impl Debug for FuncSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.linkage {
            LinkageSpecifier::Unspecified => write!(f, "linkage(auto)")?,
            LinkageSpecifier::Static => write!(f, "static")?,
            LinkageSpecifier::Extern => write!(f, "extern")?,
        }
        if self.inline {
            write!(f, " inline")?;
        }
        Ok(())
    }
}

impl Debug for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro const_() {
            if self.is_const {
                "const "
            } else {
                ""
            }
        }
        macro volatile() {
            if self.is_volatile {
                "volatile "
            } else {
                ""
            }
        }
        match &self.kind {
            &TyKind::Int(sign, size) => {
                if sign == Signness::Unsigned {
                    write!(f, "unsigned ")?;
                }
                match size {
                    IntSize::_8 => write!(f, "{}{}char", const_!(), volatile!()),
                    IntSize::_16 => write!(f, "{}{}short", const_!(), volatile!()),
                    IntSize::_32 => write!(f, "{}{}int", const_!(), volatile!()),
                    IntSize::_64 => write!(f, "{}{}long", const_!(), volatile!()),
                }
            }
            TyKind::Float(FloatSize::LongDouble) => {
                write!(f, "{}{}long double", const_!(), volatile!())
            }
            TyKind::Float(FloatSize::_64) => write!(f, "{}{}double", const_!(), volatile!()),
            TyKind::Float(FloatSize::_32) => write!(f, "{}{}float", const_!(), volatile!()),
            TyKind::Bool => write!(f, "{}{}_Bool", const_!(), volatile!()),
            TyKind::Ptr(restrict, pointee) => write!(
                f,
                "{pointee:?}*{}{}{}",
                if self.is_const { " const" } else { "" },
                if self.is_volatile { " volatile" } else { "" },
                if restrict.is_restrict() {
                    " restrict"
                } else {
                    ""
                },
            ),
            TyKind::FixedArr(ty, len) => write!(f, "{}{}{ty:?}[{len:?}]", const_!(), volatile!()),
            TyKind::Struct(name) => write!(f, "{}{}struct {name}", const_!(), volatile!()),
            TyKind::Union(name) => write!(f, "{}{}union {name}", const_!(), volatile!()),
            TyKind::Enum(name) => write!(f, "{}{}enum {name}", const_!(), volatile!()),
            TyKind::Typename(name) => write!(f, "{}{}typename {name}", const_!(), volatile!()),
            TyKind::Void => write!(f, "{}{}void", const_!(), volatile!()),
            TyKind::Error => write!(f, "{}{}ERROR", const_!(), volatile!()),
        }
    }
}

/// Used in decl list.
#[derive(Debug, Clone, PartialEq)]
pub enum DeclItem {
    Var(VarSpecifier, Spanned<Ty>, IdentStr),
    Func(FuncSpecifier, Spanned<Ty>, Signature, IdentStr),
}
