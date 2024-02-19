#![allow(dead_code)]
use std::{
    fmt::{self, Debug},
    rc::Rc,
};

use crate::{
    error::{Span, Spanned},
    token::NumValue,
    utils::{DoInBetween, IdentStr},
};

type ChildExpr = Spanned<Box<Expr>>;
pub type ExprBlock = Vec<Spanned<Expr>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// For propagating errors forward.
    Error,
    Ident(IdentStr),
    NumLiteral(NumValue),
    CharLiteral(u8),
    StrLiteral(Rc<[u8]>),
    PrefixOp(PrefixOpKind, ChildExpr),
    PostfixOp(ChildExpr, PostfixOpKind),
    /// Not including or memeber accessing because the LHS/RHS for those aren't really expressions.
    InfixOp(ChildExpr, InfixOpKind, ChildExpr),
    OpAssign(ChildExpr, AssignOpKind, ChildExpr),
    Decl(DeclItem),
    DeclList(Vec<DeclItem>),
    EmptyDecl(Ty),
    Return(Option<ChildExpr>),
    Labal(IdentStr),
    Goto(IdentStr),
    Typecast(Ty, ChildExpr),
    Call(ChildExpr, Spanned<ExprBlock>),
    Subscript(ChildExpr, ChildExpr),
    Break,
    Continue,
    Case(ChildExpr),
    Default,
    /// Representation for member access expressions.
    /// `expr.field0->field1`.
    FieldPath(ChildExpr, Vec<FieldPathItem>),
    While(ChildExpr, ExprOrBlock),
    DoWhile(ExprOrBlock, ChildExpr),
    Switch(ChildExpr, ExprOrBlock),
    For(
        Option<ChildExpr>,
        Option<ChildExpr>,
        Option<ChildExpr>,
        ExprOrBlock,
    ),
    If(Vec<(ChildExpr, ExprOrBlock)>, Option<ExprOrBlock>),
    List(Vec<ListItem>),
    Tenary(ChildExpr, ChildExpr, ChildExpr),
}

impl Expr {
    pub fn allow_omit_semicolon(&self) -> bool {
        match self {
            Self::Decl(DeclItem::Func(.., body)) => body.is_some(),
            Self::While(_, body) | Self::Switch(_, body) | Self::For(.., body) => body.is_block(),
            Self::If(elifs, else_) => {
                if let Some(else_) = else_ {
                    else_.is_block()
                } else {
                    elifs.last().unwrap().1.is_block()
                }
            }
            Self::Error | Self::Labal(..) | Self::Case(..) | Self::Default => true,
            _ => false,
        }
    }
}

/// Unrelated to the term "kind" in FP languages.
/// A type is made of a `TyKind` wrapped with `const`ness and `volatile`ness.
#[derive(Clone, PartialEq)]
pub enum TyKind {
    Int(Signness, IntSize),
    Float(FloatSize),
    Bool,
    Ptr(Restrictness, Box<Ty>),
    FixedArr(Box<Ty>, u64),
    Struct(Option<IdentStr>, Option<StructFields>),
    Union(Option<IdentStr>, Option<StructFields>),
    Enum(Option<IdentStr>, Option<EnumFields>),
    Void,
    Error,
}
pub type StructFields = Vec<(Ty, IdentStr)>;
pub type EnumFields = Vec<(IdentStr, Option<ChildExpr>)>;
impl TyKind {
    pub fn to_ty(self, is_const: bool, is_volatile: bool, typename: Option<IdentStr>) -> Ty {
        Ty {
            kind: self,
            is_const,
            is_volatile,
            typename,
        }
    }
}
#[derive(Clone, PartialEq)]
pub struct Ty {
    pub is_const: bool,
    pub is_volatile: bool,
    pub kind: TyKind,
    /// Is this type expressed as a typename?
    pub typename: Option<IdentStr>,
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
                typename: self.typename,
            },
            _ => self,
        }
    }
    /// Shorthand for making an `ERROR` (not `const`, not `volatile`, no typename) type.
    pub const fn error() -> Self {
        Self {
            is_const: false,
            is_volatile: false,
            kind: TyKind::Error,
            typename: None,
        }
    }
    /// Shorthand for making a `void` (not `const`, not `volatile`, no typename) type.
    pub const fn void() -> Self {
        Self {
            is_const: false,
            is_volatile: false,
            kind: TyKind::Void,
            typename: None,
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
    Default,
    Static,
    Extern,
}
impl LinkageSpecifier {
    pub const fn is_staic(self) -> bool {
        matches!(self, Self::Static)
    }
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
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    And,
    Or,
    Gt,
    Ge,
    Lt,
    Le,
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
    Rem,
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
            AssignOpKind::Rem => InfixOpKind::Rem,
            AssignOpKind::BitAnd => InfixOpKind::BitAnd,
            AssignOpKind::BitOr => InfixOpKind::BitOr,
            AssignOpKind::BitXor => InfixOpKind::BitXor,
            AssignOpKind::Gr => InfixOpKind::Gt,
            AssignOpKind::GrEq => InfixOpKind::Ge,
            AssignOpKind::Le => InfixOpKind::Lt,
            AssignOpKind::LeEq => InfixOpKind::Le,
            AssignOpKind::Eq => InfixOpKind::Eq,
            AssignOpKind::Ne => InfixOpKind::Ne,
            AssignOpKind::Comma => InfixOpKind::Comma,
            AssignOpKind::Bsl => InfixOpKind::Bsl,
            AssignOpKind::Bsr => InfixOpKind::Bsr,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ListItem {
    Expr(ChildExpr),
    /// `.field = expr`
    Field(Spanned<IdentStr>, ChildExpr),
    /// `[i] = expr`
    Index(ChildExpr, ChildExpr),
}

#[derive(Clone, PartialEq)]
pub struct Signature {
    pub ret_ty: Spanned<Ty>,
    pub args: Vec<(Ty, Option<Spanned<IdentStr>>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclItem {
    Var(
        VarSpecifier,
        Spanned<Ty>,
        Spanned<IdentStr>,
        Option<ChildExpr>,
    ),
    Func(
        FuncSpecifier,
        Signature,
        Spanned<IdentStr>,
        Option<ExprBlock>,
    ),
}

#[derive(Clone, Copy, PartialEq)]
pub enum FieldPathItem {
    /// Direct member accessing (`.`).
    /// Includes span for the `.` token.
    Dir(Span, Spanned<IdentStr>),
    /// Indirect member accessing (`->`).
    /// Includes span for the `->` token.
    Ind(Span, Spanned<IdentStr>),
}

#[derive(Clone, PartialEq)]
pub enum ExprOrBlock {
    Expr(Box<Spanned<Expr>>),
    Block(Spanned<ExprBlock>),
}
impl From<Box<Spanned<Expr>>> for ExprOrBlock {
    fn from(value: Box<Spanned<Expr>>) -> Self {
        Self::Expr(value)
    }
}
impl From<Spanned<Expr>> for ExprOrBlock {
    fn from(value: Spanned<Expr>) -> Self {
        Box::new(value).into()
    }
}
impl From<Spanned<ExprBlock>> for ExprOrBlock {
    fn from(value: Spanned<ExprBlock>) -> Self {
        Self::Block(value)
    }
}
impl ExprOrBlock {
    pub const fn span(&self) -> Span {
        match self {
            Self::Expr(expr) => expr.span(),
            Self::Block(block) => block.span(),
        }
    }
    pub const fn is_block(&self) -> bool {
        matches!(self, Self::Block(..))
    }
    pub const fn is_expr(&self) -> bool {
        matches!(self, Self::Expr(..))
    }
}

impl Debug for ExprOrBlock {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Expr(expr) => Debug::fmt(expr, f),
            Self::Block(block) => Debug::fmt(block, f),
        }
    }
}

impl Debug for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{:?} (", self.ret_ty)?;
        } else {
            write!(f, "{:?}(", self.ret_ty)?;
        }
        let f = self.args.iter().try_do_in_between(
            f,
            |f| {
                if f.alternate() {
                    write!(f, ", ")
                } else {
                    write!(f, ",")
                }
            },
            |f, (ty, name)| match name {
                Some(name) => write!(f, "{ty:?} {name:?}"),
                None => write!(f, "{ty:?}"),
            },
        )?;
        write!(f, ")")
    }
}

impl Debug for FuncSpecifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.linkage {
            LinkageSpecifier::Default => write!(f, "linkage(default)")?,
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
    fn fmt(&self, mut f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        macro write_struct_union_fields($fields:expr) {
            match $fields {
                Some(fields) => {
                    write!(f, " {{")?;
                    f = fields.iter().try_do_in_between(
                        f,
                        |f| {
                            if f.alternate() {
                                write!(f, "; ")
                            } else {
                                write!(f, ";")
                            }
                        },
                        |f, (ty, name)| write!(f, "{ty:?} {name:?}"),
                    )?;
                    write!(f, "}}")?;
                }
                None => (),
            }
        }
        match &self.kind {
            &TyKind::Int(sign, size) => {
                if sign == Signness::Unsigned {
                    write!(f, "unsigned ")?;
                }
                match size {
                    IntSize::_8 => write!(f, "{}{}char", const_!(), volatile!())?,
                    IntSize::_16 => write!(f, "{}{}short", const_!(), volatile!())?,
                    IntSize::_32 => write!(f, "{}{}int", const_!(), volatile!())?,
                    IntSize::_64 => write!(f, "{}{}long", const_!(), volatile!())?,
                }
            }
            TyKind::Float(FloatSize::LongDouble) => {
                write!(f, "{}{}long double", const_!(), volatile!())?
            }
            TyKind::Float(FloatSize::_64) => write!(f, "{}{}double", const_!(), volatile!())?,
            TyKind::Float(FloatSize::_32) => write!(f, "{}{}float", const_!(), volatile!())?,
            TyKind::Bool => write!(f, "{}{}_Bool", const_!(), volatile!())?,
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
            )?,
            TyKind::FixedArr(ty, len) => write!(f, "{}{}{ty:?}[{len:?}]", const_!(), volatile!())?,
            TyKind::Struct(Some(name), fields) => {
                write!(f, "{}{}struct {name:?}", const_!(), volatile!())?;
                write_struct_union_fields!(fields)
            }
            TyKind::Struct(None, fields) => {
                write!(f, "{}{}struct", const_!(), volatile!())?;
                write_struct_union_fields!(fields)
            }
            TyKind::Union(Some(name), fields) => {
                write!(f, "{}{}union {name:?}", const_!(), volatile!())?;
                write_struct_union_fields!(fields)
            }
            TyKind::Union(None, fields) => {
                write!(f, "{}{}union", const_!(), volatile!())?;
                write_struct_union_fields!(fields)
            }
            TyKind::Enum(name, fields) => {
                write!(f, "{}{}enum", const_!(), volatile!())?;
                if let Some(name) = name {
                    write!(f, " {:?}", name)?;
                }
                match fields {
                    Some(fields) => {
                        write!(f, " {{")?;
                        f = fields.iter().try_do_in_between(
                            f,
                            |f| {
                                if f.alternate() {
                                    write!(f, ", ")
                                } else {
                                    write!(f, ",")
                                }
                            },
                            |f, (ty, name)| write!(f, "{ty:?} {name:?}"),
                        )?;
                        write!(f, "}}")?;
                    }
                    None => (),
                }
            }
            TyKind::Void => write!(f, "{}{}void", const_!(), volatile!())?,
            TyKind::Error => write!(f, "{}{}ERROR", const_!(), volatile!())?,
        }
        match self.typename {
            Some(typename) => write!(f, " (aka. {typename:?})"),
            None => Ok(()),
        }
    }
}

impl Debug for FieldPathItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Dir(_, ident) => write!(f, "FieldPathItem(.{ident:?})"),
            Self::Ind(_, ident) => write!(f, "FieldPathItem(->{ident:?})"),
        }
    }
}
