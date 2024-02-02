#![allow(dead_code)]
use std::{
    fmt::{self, Debug},
    iter::Peekable,
    rc::Rc,
};

use crate::{
    error::{Error, ErrorReporter, Span, Spanned, ToSpanned},
    intern_str::InternStr,
    token::NumValue,
    utils::{fixme, match_into, match_into_unchecked},
};

#[allow(unused_imports)]
use super::token::{Token, TokenStream};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// For propagating erors forward.
    Error,
    Ident(InternStr<'static>),
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
    VarDecl(VarSpecifier, Spanned<Ty>, InternStr<'static>),
    VarDeclInit(
        VarSpecifier,
        Spanned<Ty>,
        InternStr<'static>,
        Box<Spanned<Expr>>,
    ),
    /// Fuck C.
    DeclList(Vec<DeclItem>),
    Return(Option<Box<Spanned<Expr>>>),
    FuncDef(
        FuncSpecifier,
        InternStr<'static>,
        Signature,
        Option<Vec<Spanned<Expr>>>,
    ),
    Labal(InternStr<'static>),
    Goto(InternStr<'static>),
    Break,
    Continue,
}
#[derive(Clone, PartialEq, Eq)]
pub enum TyKind {
    Int(Signness, IntSize),
    Float(FloatSize),
    Bool,
    Ptr(Restrictness, Box<Ty>),
    FixedArr(Box<Ty>, u64),
    Struct(InternStr<'static>),
    Union(InternStr<'static>),
    Enum(InternStr<'static>),
    Typename(InternStr<'static>),
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
/// See `DeclSpecifiers` for more info on the terms used in LMCC to describe specifiers.
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
}
/// See `DeclSpecifiers` for more info on the terms used in LMCC to describe specifiers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LinkageSpecifier {
    Unspecified,
    Static,
    Extern,
}
/// Keywords that appears before a function declaration.
/// Note that this term mean a different thing from the same term used in documents of C standards.
/// See `DeclSpecifiers` for more info on the terms used in LMCC to describe specifiers.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct FuncSpecifier {
    inline: bool,
    linkage: LinkageSpecifier,
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
    ret_ty: Spanned<Ty>,
    args: Vec<(Ty, Option<InternStr<'static>>)>,
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
    Var(VarSpecifier, Spanned<Ty>, InternStr<'static>),
    Func(FuncSpecifier, Spanned<Ty>, Signature, InternStr<'static>),
}

#[derive(Debug, Clone)]
pub struct Parser {
    err_reporter: Rc<ErrorReporter>,
    tokens: Peekable<TokenStream>,
}

macro next_token_if_matches($token_stream:expr, $pat:pat $(,)?) {{
    $token_stream.next_if(|t| matches!(t.inner(), $pat))
}}

/// Only works in `parse_` functions (`(self: &mut Parser, ...) -> Option<...>`).
/// Returns `Some((InternStr<'static>, Span))` if is an identifier.
/// Returns `None` if not an ident.
macro expect_ident($self:expr, $prev_span:expr) {{
    let token = $self.expect_next_token($prev_span)?;
    let span = token.span();
    if let Some(ident) = match_into!(token.into_inner(), Token::Ident(ident) => ident) {
        Some((ident, span))
    } else {
        $self.err_reporter
            .report(&Error::ExpectIdent.to_spanned(span));
        None
    }
}}

/// The union set of function specifers and variable specifiers.
/// LMCC uses slightly different terms to those used in the documents of C standards.
///
/// - **variable specifer** is keywords that appear before a variable declaration.
/// - **function specifer** is keywords that appear before a function declaration (linkage specifiers and `inline`).
/// - **declaration specifier** is the union set of function and variable specifiers.
///
/// `DeclSpecifiers` are only used during parsering, they are reduced to function or variable specifiers afterwards,
/// reporting invalid combinations of specifiers (if any) during the reduction.
#[derive(Debug, Clone, Copy, PartialEq, Default)]
struct DeclSpecifiers {
    static_: Option<Span>,
    inline: Option<Span>,
    extern_: Option<Span>,
    register: Option<Span>,
    auto: Option<Span>,
}

impl DeclSpecifiers {
    fn set_static(&mut self, span: Span, err_reporter: &ErrorReporter) {
        if let Some(span) = self.static_ {
            err_reporter.report(&Error::DuplicateSpecifier.to_spanned(span));
        } else {
            self.static_ = Some(span);
        }
    }
    fn set_inline(&mut self, span: Span, err_reporter: &ErrorReporter) {
        if let Some(span) = self.inline {
            err_reporter.report(&Error::DuplicateSpecifier.to_spanned(span));
        } else {
            self.inline = Some(span);
        }
    }
    fn set_extern(&mut self, span: Span, err_reporter: &ErrorReporter) {
        if let Some(span) = self.extern_ {
            err_reporter.report(&Error::DuplicateSpecifier.to_spanned(span));
        } else {
            self.extern_ = Some(span);
        }
    }
    fn set_register(&mut self, span: Span, err_reporter: &ErrorReporter) {
        if let Some(span) = self.register {
            err_reporter.report(&Error::DuplicateSpecifier.to_spanned(span));
        } else {
            self.register = Some(span);
        }
    }
    fn set_auto(&mut self, span: Span, err_reporter: &ErrorReporter) {
        if let Some(span) = self.auto {
            err_reporter.report(&Error::DuplicateSpecifier.to_spanned(span));
        } else {
            self.auto = Some(span);
        }
    }
    /// For invalid combinations of specifiers, report the error and return a guess of what the user meant.
    fn try_into_var_speci(self, err_reporter: &ErrorReporter) -> VarSpecifier {
        macro ensure_no($keyword:tt, $err:tt $(,)?) {
            self.$keyword
                .inspect(|&span| err_reporter.report(&Error::$err.to_spanned(span)))
        }
        if self.static_.is_some() {
            ensure_no!(register, ConflictingStorageClass);
            ensure_no!(extern_, ConflictingStorageClass);
            ensure_no!(auto, ConflictingStorageClass);
            VarSpecifier::Static
        } else if self.register.is_some() {
            ensure_no!(static_, ConflictingStorageClass);
            ensure_no!(extern_, ConflictingStorageClass);
            ensure_no!(auto, ConflictingStorageClass);
            VarSpecifier::Register
        } else if self.extern_.is_some() {
            ensure_no!(static_, ConflictingStorageClass);
            ensure_no!(register, ConflictingStorageClass);
            ensure_no!(auto, ConflictingStorageClass);
            VarSpecifier::Extern
        } else if self.auto.is_some() {
            ensure_no!(static_, ConflictingStorageClass);
            ensure_no!(register, ConflictingStorageClass);
            ensure_no!(extern_, ConflictingStorageClass);
            VarSpecifier::Auto
        } else {
            VarSpecifier::Unspecified
        }
    }
    /// For invalid combinations of specifiers, report the error and return a guess of what the user meant.
    fn try_into_func_speci(self, err_reporter: &ErrorReporter) -> FuncSpecifier {
        macro ensure_no($keyword:tt, $err:tt $(,)?) {
            self.$keyword
                .inspect(|&span| err_reporter.report(&Error::$err.to_spanned(span)))
        }
        ensure_no!(register, InvalidStorageClassOnFunc);
        ensure_no!(auto, InvalidStorageClassOnFunc);
        let linkage = if self.extern_.is_some() {
            ensure_no!(static_, ConflictingStorageClass);
            LinkageSpecifier::Extern
        } else if self.static_.is_some() {
            ensure_no!(extern_, ConflictingStorageClass);
            LinkageSpecifier::Static
        } else {
            LinkageSpecifier::Unspecified
        };
        FuncSpecifier {
            inline: self.inline.is_some(),
            linkage,
        }
    }
}

impl Parser {
    pub fn new(err_reporter: Rc<ErrorReporter>, tokens: Peekable<TokenStream>) -> Self {
        Self {
            err_reporter,
            tokens,
        }
    }

    /// If token stream ends, reports `UnexpectedEOF` and return `None`.
    fn expect_next_token(&mut self, prev_span: Span) -> Option<Spanned<Token>> {
        self.tokens
            .next()
            .ok_or(Error::UnexpectedEof.to_spanned((prev_span.file, prev_span.end)))
            .inspect_err(|e| self.err_reporter.report(e))
            .ok()
    }

    /// If token stream ends, reports `UnexpectedEOF` and return `None`.
    fn expect_peek_token(&mut self, prev_span: Span) -> Option<&Spanned<Token>> {
        self.tokens
            .peek()
            .ok_or(Error::UnexpectedEof.to_spanned((prev_span.file, prev_span.end)))
            .inspect_err(|e| self.err_reporter.report(e))
            .ok()
    }

    /// Parse type specifiers.
    fn parse_ty_speci(&mut self, prev_span: Span) -> Option<Spanned<Ty>> {
        let mut signness_flag = Option::<Signness>::None;
        let mut signness_span = Option::<Span>::None;
        let mut constness = Option::<Span>::None;
        let mut volatileness = Option::<Span>::None;
        let mut prev_span = prev_span;
        loop {
            let Some(token) = self.tokens.peek() else {
                break;
            };
            prev_span = token.span();
            match token.inner() {
                Token::Signed => {
                    self.tokens.next();
                    match signness_flag {
                        Some(Signness::Signed) => fixme!("warnings for unneeded `signed`"),
                        Some(Signness::Unsigned) => {
                            self.err_reporter
                                .report(&Error::ConflictingSignness.to_spanned(prev_span));
                        }
                        None => signness_flag = Some(Signness::Signed),
                    };
                    signness_span = Some(signness_span.unwrap_or(prev_span))
                }
                Token::Unsigned => {
                    self.tokens.next();
                    match signness_flag {
                        Some(Signness::Signed) => {
                            self.err_reporter
                                .report(&Error::ConflictingSignness.to_spanned(prev_span));
                        }
                        Some(Signness::Unsigned) => fixme!("warnings for unneeded `unsigned`"),
                        None => signness_flag = Some(Signness::Unsigned),
                    };
                    signness_span = Some(signness_span.unwrap_or(prev_span))
                }
                Token::Const => {
                    self.tokens.next();
                    if constness.is_some() {
                        fixme!("warnings for unneeded `const`")
                    }
                    constness = Some(prev_span);
                }
                Token::Volatile => {
                    self.tokens.next();
                    if volatileness.is_some() {
                        fixme!("warnings for unneeded `const`")
                    }
                    volatileness = Some(prev_span);
                }
                Token::Restrict => todo!(),
                Token::Char
                | Token::Double
                | Token::Enum
                | Token::Float
                | Token::Int
                | Token::Long
                | Token::Short
                | Token::Struct
                | Token::Union
                | Token::Void
                | Token::Bool
                | Token::Ident(..)
                | Token::Mul => break,
                Token::Complex => todo!(),
                Token::Imaginary => todo!(),
                _ => match signness_flag {
                    Some(sign) => {
                        return Some(
                            TyKind::Int(sign, IntSize::_32)
                                .to_ty(constness.is_some(), volatileness.is_some())
                                .to_spanned(signness_span.unwrap()),
                        )
                    }
                    None => {
                        self.err_reporter
                            .report(&Error::ExpectTyExpr.to_spanned(prev_span));
                        return Some(
                            TyKind::Error
                                .to_ty(constness.is_some(), volatileness.is_some())
                                .to_spanned(prev_span),
                        );
                    }
                },
            }
        }
        let signness = signness_flag.unwrap_or(Signness::Signed);
        macro report_if_has_signness() {{
            if signness_flag.is_some() {
                self.err_reporter
                    .report(&Error::InvalidSignnessFlag.to_spanned(signness_span.unwrap()));
            }
        }}
        let token = self.expect_peek_token(prev_span)?;
        let span = token.span();
        // Joined span from signness span to the end of the type expression span.
        // Does not work for pointers.
        let joined_span = signness_span.map_or(span, |s| s.join(span));
        match token.inner() {
            Token::Char => {
                self.tokens.next();
                Some(
                    TyKind::Int(signness, IntSize::_8)
                        .to_ty(constness.is_some(), volatileness.is_some())
                        .to_spanned(joined_span),
                )
            }
            Token::Short => {
                self.tokens.next();
                Some(
                    TyKind::Int(signness, IntSize::_16)
                        .to_ty(constness.is_some(), volatileness.is_some())
                        .to_spanned(joined_span),
                )
            }
            Token::Int => {
                self.tokens.next();
                Some(
                    TyKind::Int(signness, IntSize::_32)
                        .to_ty(constness.is_some(), volatileness.is_some())
                        .to_spanned(joined_span),
                )
            }
            Token::Long => {
                self.tokens.next();
                next_token_if_matches!(self.tokens, Token::Long);
                Some(
                    TyKind::Int(signness, IntSize::_64)
                        .to_ty(constness.is_some(), volatileness.is_some())
                        .to_spanned(joined_span),
                )
            }
            Token::Float => {
                self.tokens.next();
                report_if_has_signness!();
                Some(
                    TyKind::Float(FloatSize::_32)
                        .to_ty(constness.is_some(), volatileness.is_some())
                        .to_spanned(joined_span),
                )
            }
            Token::Double => {
                self.tokens.next();
                report_if_has_signness!();
                Some(
                    TyKind::Float(FloatSize::_64)
                        .to_ty(constness.is_some(), volatileness.is_some())
                        .to_spanned(joined_span),
                )
            }
            Token::Void => {
                self.tokens.next();
                report_if_has_signness!();
                Some(
                    TyKind::Void
                        .to_ty(constness.is_some(), volatileness.is_some())
                        .to_spanned(joined_span),
                )
            }
            Token::Bool => {
                self.tokens.next();
                report_if_has_signness!();
                Some(
                    TyKind::Bool
                        .to_ty(constness.is_some(), volatileness.is_some())
                        .to_spanned(joined_span),
                )
            }
            Token::Struct => {
                self.tokens.next();
                report_if_has_signness!();
                let ty = expect_ident!(self, span).map_or_else(
                    || {
                        TyKind::Error
                            .to_ty(constness.is_some(), volatileness.is_some())
                            .to_spanned(joined_span)
                    },
                    |(name, span)| {
                        TyKind::Struct(name)
                            .to_ty(constness.is_some(), volatileness.is_some())
                            .to_spanned(joined_span.join(span))
                    },
                );
                Some(ty)
            }
            Token::Enum => {
                self.tokens.next();
                report_if_has_signness!();
                let ty = expect_ident!(self, span).map_or_else(
                    || {
                        TyKind::Error
                            .to_ty(constness.is_some(), volatileness.is_some())
                            .to_spanned(joined_span)
                    },
                    |(name, span)| {
                        TyKind::Enum(name)
                            .to_ty(constness.is_some(), volatileness.is_some())
                            .to_spanned(joined_span.join(span))
                    },
                );
                Some(ty)
            }
            Token::Union => {
                self.tokens.next();
                report_if_has_signness!();
                let ty = expect_ident!(self, span).map_or_else(
                    || {
                        TyKind::Error
                            .to_ty(constness.is_some(), volatileness.is_some())
                            .to_spanned(joined_span)
                    },
                    |(name, span)| {
                        TyKind::Union(name)
                            .to_ty(constness.is_some(), volatileness.is_some())
                            .to_spanned(joined_span.join(span))
                    },
                );
                Some(ty)
            }
            &Token::Ident(name) => match signness_flag {
                Some(sign) => Some(
                    TyKind::Int(sign, IntSize::_32)
                        .to_ty(constness.is_some(), volatileness.is_some())
                        .to_spanned(signness_span.unwrap()),
                ),
                _ => {
                    self.tokens.next();
                    Some(
                        TyKind::Typename(name)
                            .to_ty(constness.is_some(), volatileness.is_some())
                            .to_spanned(joined_span),
                    )
                }
            },
            Token::Const | Token::Volatile | Token::Restrict => todo!("type declarators"),
            Token::Signed | Token::Unsigned => {
                unreachable!()
            }
            _ => match signness_flag {
                Some(s) => Some(
                    TyKind::Int(s, IntSize::_32)
                        .to_ty(constness.is_some(), volatileness.is_some())
                        .to_spanned(signness_span.unwrap()),
                ),
                None => {
                    self.err_reporter
                        .report(&Error::ExpectTyExpr.to_spanned(span));
                    Some(
                        TyKind::Error
                            .to_ty(constness.is_some(), volatileness.is_some())
                            .to_spanned(prev_span.tail()),
                    )
                }
            },
        }
    }

    /// Parse (possible) array declarators.
    fn parse_arr_decl(&mut self, mut ty: Spanned<Ty>) -> Option<Spanned<Ty>> {
        macro go_to_closing_bracket($prev_span:expr) {{
            let token = self.expect_peek_token($prev_span)?;
            let span = token.span();
            if token.inner() == &Token::BracketClose {
                self.tokens.next();
            } else {
                self.err_reporter
                    .report(&Error::ExpectToken(Token::BracketClose).to_spanned(span));
            }
            span
        }}
        while let Some(token) = self.tokens.peek() {
            let opening_bracket_span = token.span();
            match token.inner() {
                Token::BracketOpen => {
                    self.tokens.next();
                    let token = self.expect_peek_token(opening_bracket_span)?;
                    let span = token.span();
                    match token.inner() {
                        Token::BracketClose => {
                            self.tokens.next();
                            ty = TyKind::Ptr(Restrictness::NoRestrict, Box::new(ty.into_inner()))
                                .to_ty(false, false)
                                .to_spanned(span);
                        }
                        &Token::NumLiteral(NumValue::I(i)) => {
                            self.tokens.next();
                            let prev_ty_span = ty.span();
                            let closing_bracket_span = go_to_closing_bracket!(span);
                            if let Ok(i) = u64::try_from(i) {
                                ty = TyKind::FixedArr(Box::new(ty.into_inner()), i)
                                    .to_ty(false, false)
                                    .to_spanned(prev_ty_span.join(closing_bracket_span));
                            } else {
                                self.err_reporter
                                    .report(&Error::IllegalArrLen.to_spanned(span));
                                ty = TyKind::Error
                                    .to_ty(false, false)
                                    .to_spanned(prev_ty_span.join(closing_bracket_span));
                            }
                        }
                        Token::NumLiteral(NumValue::F(_)) => {
                            let prev_ty_span = ty.span();
                            let closing_bracket_span = go_to_closing_bracket!(span);
                            self.err_reporter
                                .report(&Error::IllegalArrLen.to_spanned(span));
                            ty = TyKind::Error
                                .to_ty(false, false)
                                .to_spanned(prev_ty_span.join(closing_bracket_span));
                        }
                        _ => {
                            self.err_reporter
                                .report(&Error::Todo("variable array length").to_spanned(span));
                            let span = self.parse_expr(opening_bracket_span, 16)?.span();
                            let prev_ty_span = ty.span();
                            let closing_bracket_span = go_to_closing_bracket!(span);
                            ty = TyKind::Error
                                .to_ty(false, false)
                                .to_spanned(prev_ty_span.join(closing_bracket_span));
                            break;
                        }
                    }
                }
                _ => break,
            }
        }
        return Some(ty);
    }

    /// Parse (possible) pointer declarators.
    fn parse_ptr_decl(&mut self, mut ty: Spanned<Ty>) -> Option<Spanned<Ty>> {
        while let Some(token) = self.tokens.peek() {
            let span = token.span();
            match token.inner() {
                Token::Mul => {
                    self.tokens.next();
                    let mut restrict = Option::<Span>::None;
                    let mut const_ = Option::<Span>::None;
                    let mut volatile = Option::<Span>::None;
                    while let Some(token) = self.tokens.peek() {
                        let span = token.span();
                        match token.inner() {
                            Token::Restrict => {
                                self.tokens.next();
                                if restrict.is_some() {
                                    fixme!("warning for unneeded `restrict`")
                                }
                                restrict = Some(span);
                            }
                            Token::Const => {
                                self.tokens.next();
                                if const_.is_some() {
                                    fixme!("warning for unneeded `const`")
                                }
                                const_ = Some(span);
                            }
                            Token::Volatile => {
                                self.tokens.next();
                                if volatile.is_some() {
                                    fixme!("warning for unneeded `volatile`")
                                }
                                volatile = Some(span);
                            }
                            _ => break,
                        }
                    }
                    let span = ty.span().join(span);
                    ty = TyKind::Ptr(
                        if restrict.is_some() {
                            Restrictness::Restrict
                        } else {
                            Restrictness::NoRestrict
                        },
                        Box::new(ty.into_inner()),
                    )
                    .to_ty(const_.is_some(), volatile.is_some())
                    .to_spanned(span);
                }
                _ => break,
            }
        }
        Some(ty)
    }

    /// Parse a type, including both type specifiers and pointer declarators.
    /// Starting from ParenOpen already consumed.
    fn parse_ty_expr(&mut self, prev_span: Span) -> Option<Spanned<Ty>> {
        let ty = self.parse_ty_speci(prev_span)?;
        let ty = self.parse_ptr_decl(ty)?;
        Some(ty)
    }

    fn parse_fn_decl_args(
        &mut self,
        prev_span: Span,
    ) -> Option<Vec<(Ty, Option<InternStr<'static>>)>> {
        let mut args = Vec::<(Ty, Option<InternStr<'static>>)>::new();
        let token = self.expect_peek_token(prev_span)?;
        if token.inner() == &Token::ParenClose {
            self.tokens.next();
        } else {
            loop {
                let mut prev_span = prev_span;
                let mut ty = self.parse_ty_expr(prev_span)?;
                let token = self.expect_peek_token(prev_span)?;
                let ident_span = token.span();
                // Handle possible anon args.
                let ident = match token.inner() {
                    Token::ParenClose => {
                        self.tokens.next();
                        args.push((ty.into_inner(), None));
                        break;
                    }
                    Token::Comma => {
                        self.tokens.next();
                        args.push((ty.into_inner(), None));
                        continue;
                    }
                    &Token::Ident(ident) => {
                        self.tokens.next();
                        ty = self.parse_arr_decl(ty)?;
                        if ty.is_arr() {
                            self.err_reporter
                                .report(&Error::ArrInFuncArg.to_spanned(ty.span()));
                            ty = ty.map(|t| t.decayed());
                        }
                        ident
                    }
                    _ => {
                        self.err_reporter
                            .report(&Error::ExpectIdent.to_spanned(ident_span));
                        return None;
                    }
                };
                prev_span = ident_span;
                args.push((ty.into_inner(), Some(ident)));
                let token = self.expect_peek_token(prev_span)?;
                let span = token.span();
                match token.inner() {
                    Token::ParenClose => {
                        self.tokens.next();
                        break;
                    }
                    Token::Comma => {
                        self.tokens.next();
                    }
                    _ => {
                        self.err_reporter.report(
                            &Error::ExpectTokens(&[Token::ParenClose, Token::Comma])
                                .to_spanned(span),
                        );
                        todo!("find a place to restart parsing here");
                    }
                }
            }
        }
        let token = self.tokens.peek();
        match token {
            Some(t) if t.inner() == &Token::BraceOpen => {
                todo!("parse function body");
            }
            Some(..) | None => Some(args),
        }
    }

    /// Parse an entire decl statement.
    fn parse_decl_stmt(&mut self, prev_span: Span) -> Option<Spanned<Expr>> {
        // Parse decl specifiers.
        let mut decl_speci = DeclSpecifiers::default();
        while let Some(token) = self.expect_peek_token(prev_span) {
            match token.inner() {
                Token::Static => {
                    let span = token.span();
                    self.tokens.next();
                    decl_speci.set_static(span, &self.err_reporter);
                }
                Token::Inline => {
                    let span = token.span();
                    self.tokens.next();
                    decl_speci.set_inline(span, &self.err_reporter);
                }
                Token::Extern => {
                    let span = token.span();
                    self.tokens.next();
                    decl_speci.set_extern(span, &self.err_reporter);
                }
                Token::Register => {
                    let span = token.span();
                    self.tokens.next();
                    decl_speci.set_register(span, &self.err_reporter);
                }
                Token::Auto => {
                    let span = token.span();
                    self.tokens.next();
                    decl_speci.set_auto(span, &self.err_reporter);
                }
                _ => break,
            }
        }

        // Parse type specifiers.
        let ty = self.parse_ty_expr(prev_span)?;
        let ty_span = ty.span();
        let (ident, ident_span) = expect_ident!(self, ty_span)?;
        // Parse array decl.
        let ty = self.parse_arr_decl(ty)?;

        // Parse RHS.
        match self.tokens.peek() {
            Some(t) if t.inner() == &Token::Eq => {
                self.tokens.next();
                let var_specs = decl_speci.try_into_var_speci(&self.err_reporter);
                let rhs = self.parse_expr(ident_span, 16)?;
                let rhs_span = rhs.span();
                Some(
                    Expr::VarDeclInit(var_specs, ty, ident, Box::new(rhs))
                        .to_spanned(ty_span.join(rhs_span)),
                )
            }
            Some(t) if t.inner() == &Token::Comma => todo!(),
            Some(t) if t.inner() == &Token::ParenOpen => {
                let ret_ty = if ty.is_arr() {
                    self.err_reporter
                        .report(&Error::ArrAsFuncRet.to_spanned(ty.span()));
                    ty.map(|t| t.decayed())
                } else {
                    ty
                };
                self.tokens.next();
                let args = self.parse_fn_decl_args(ident_span)?;
                Some(
                    Expr::FuncDef(
                        decl_speci.try_into_func_speci(&self.err_reporter),
                        ident,
                        Signature { ret_ty, args },
                        None,
                    )
                    .to_spanned(ty_span.join(prev_span)),
                )
            }
            _ => {
                let var_specs = decl_speci.try_into_var_speci(&self.err_reporter);
                Some(Expr::VarDecl(var_specs, ty, ident).to_spanned(ty_span.join(ident_span)))
            }
        }
    }

    fn parse_expr(&mut self, prev_span: Span, _prec: u16) -> Option<Spanned<Expr>> {
        let token = self.expect_peek_token(prev_span)?;
        let span = token.span();
        macro parse_prefix_op($prec:expr, $opkind:expr $(,)?) {{
            self.tokens.next();
            let operand = self.parse_expr(span, $prec)?;
            let operand_span = operand.span();
            Some(Expr::PrefixOp($opkind, Box::new(operand)).to_spanned(span.join(operand_span)))
        }}
        match token.inner() {
            &Token::Ident(s) => {
                self.tokens.next();
                match self.tokens.peek() {
                    Some(t) if t.inner() == &Token::Colon => {
                        self.tokens.next();
                        Some(Expr::Labal(s).to_spanned(span))
                    }
                    Some(t) if matches!(t.inner(), &Token::Ident(..)) => {
                        todo!();
                    }
                    _ => Some(Expr::Ident(s).to_spanned(span)),
                }
            }
            &Token::NumLiteral(n) => {
                self.tokens.next();
                Some(Expr::NumLiteral(n).to_spanned(span))
            }
            Token::StrLiteral(..) => {
                let bytes = unsafe {
                    match_into_unchecked!(self.tokens.next().unwrap_unchecked().into_inner(), Token::StrLiteral(s) => s)
                };
                Some(Expr::StrLiteral(bytes).to_spanned(span))
            }
            &Token::CharLiteral(c) => {
                self.tokens.next();
                Some(Expr::CharLiteral(c).to_spanned(span))
            }
            Token::Char
            | Token::Const
            | Token::Double
            | Token::Enum
            | Token::Float
            | Token::Int
            | Token::Long
            | Token::Restrict
            | Token::Short
            | Token::Signed
            | Token::Struct
            | Token::Union
            | Token::Unsigned
            | Token::Void
            | Token::Volatile
            | Token::Bool
            | Token::Complex
            | Token::Imaginary
            | Token::Auto
            | Token::Register
            | Token::Extern
            | Token::Static
            | Token::Inline => self.parse_decl_stmt(span),
            Token::Break => {
                self.tokens.next();
                Some(Expr::Break.to_spanned(span))
            }
            Token::Case => todo!(),
            Token::Continue => {
                self.tokens.next();
                Some(Expr::Continue.to_spanned(span))
            }
            Token::Default => todo!(),
            Token::Do => todo!(),
            Token::Else => todo!(),
            Token::For => todo!(),
            Token::Goto => {
                self.tokens.next();
                let Some((ident, ident_span)) = expect_ident!(self, span) else {
                    return Some(Expr::Error.to_spanned(span));
                };
                Some(Expr::Goto(ident).to_spanned(span.join(ident_span)))
            }
            Token::If => todo!(),
            Token::Return => {
                self.tokens.next();
                match self.tokens.peek() {
                    Some(t) if matches!(t.inner(), Token::Semicolon | Token::Comma) => {
                        Some(Expr::Return(None).to_spanned(span))
                    }
                    None => Some(Expr::Return(None).to_spanned(span)),
                    Some(..) => {
                        self.tokens.next();
                        let operand = self.parse_expr(span, 16)?;
                        let operand_span = operand.span();
                        Some(
                            Expr::Return(Some(Box::new(operand)))
                                .to_spanned(span.join(operand_span)),
                        )
                    }
                }
            }
            Token::Sizeof => todo!(),
            Token::Switch => todo!(),
            Token::Typedef => todo!(),
            Token::While => todo!(),
            Token::Add => parse_prefix_op!(2, PrefixOpKind::UnaryAdd),
            Token::AddAdd => parse_prefix_op!(2, PrefixOpKind::PreInc),
            Token::AddEq => todo!(),
            Token::Sub => parse_prefix_op!(2, PrefixOpKind::UnarySub),
            Token::SubSub => parse_prefix_op!(2, PrefixOpKind::PreDec),
            Token::SubEq => todo!(),
            Token::Arrow => todo!(),
            Token::Mul => parse_prefix_op!(2, PrefixOpKind::Ref),
            Token::MulEq => todo!(),
            Token::Div => todo!(),
            Token::DivEq => todo!(),
            Token::Rem => todo!(),
            Token::RemEq => todo!(),
            Token::Lt => todo!(),
            Token::LtLt => todo!(),
            Token::LtLtEq => todo!(),
            Token::LtEq => todo!(),
            Token::Gt => todo!(),
            Token::GtGt => todo!(),
            Token::GtGtEq => todo!(),
            Token::GtEq => todo!(),
            Token::Tilde => parse_prefix_op!(2, PrefixOpKind::BitNot),
            Token::Exc => parse_prefix_op!(2, PrefixOpKind::Not),
            Token::ExcEq => todo!(),
            Token::And => parse_prefix_op!(2, PrefixOpKind::Ref),
            Token::AndAnd => {
                self.tokens.next();
                // `&&` can be used to take reference twice.
                // The outer `&` would be invalid (since the value produced by `&expr` is an rvalue),
                // but error would be reported later down the line.
                let inner_operand = self.parse_expr(span, 2)?;
                let inner_span = inner_operand.span();
                let outer_operand = Expr::PrefixOp(PrefixOpKind::Ref, Box::new(inner_operand))
                    .to_spanned((span.file, span.start + 1, inner_span.end));
                Some(
                    Expr::PrefixOp(PrefixOpKind::Ref, Box::new(outer_operand)).to_spanned((
                        span.file,
                        span.start,
                        inner_span.end,
                    )),
                )
            }
            Token::AndEq => todo!(),
            Token::Eq => todo!(),
            Token::EqEq => todo!(),
            Token::Or => todo!(),
            Token::OrOr => todo!(),
            Token::OrEq => todo!(),
            Token::Xor => todo!(),
            Token::XorEq => todo!(),
            Token::Dot => todo!(),
            Token::Comma => todo!(),
            Token::Colon => todo!(),
            Token::Semicolon => todo!(),
            Token::Ques => todo!(),
            Token::ParenOpen => todo!(),
            Token::ParenClose => todo!(),
            Token::BracketOpen => todo!(),
            Token::BracketClose => todo!(),
            Token::BraceOpen => todo!(),
            Token::BraceClose => todo!(),
            Token::Backslash => todo!(),
        }
    }
}

impl Iterator for Parser {
    type Item = Spanned<Expr>;

    fn next(&mut self) -> Option<Self::Item> {
        let span = self.tokens.peek()?.span();
        self.parse_expr(span, 16)
    }
}
