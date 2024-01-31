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
    VarDecl(StorageSpecifier, TyExpr, InternStr<'static>),
    VarDeclAssign(
        StorageSpecifier,
        TyExpr,
        Spanned<InternStr<'static>>,
        Box<Spanned<Expr>>,
    ),
    Return(Box<Spanned<Expr>>),
    FnDef(InternStr<'static>, Signature, Option<Vec<Spanned<Expr>>>),
    Labal(InternStr<'static>),
    Goto(InternStr<'static>),
    Break,
    Continue,
}
impl ToSpanned for Expr {}
#[derive(Clone, PartialEq)]
pub enum TyExpr {
    Int(Signness, IntSize),
    Float(FloatSize),
    Bool,
    Ptr(Box<TyExpr>),
    Struct(InternStr<'static>),
    Union(InternStr<'static>),
    Enum(InternStr<'static>),
    Typename(InternStr<'static>),
    Void,
    Unknown,
}
impl Debug for TyExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            &Self::Int(sign, size) => {
                if sign == Signness::Unsigned {
                    write!(f, "unsigned ")?;
                }
                match size {
                    IntSize::_8 => write!(f, "char"),
                    IntSize::_16 => write!(f, "short"),
                    IntSize::_32 => write!(f, "int"),
                    IntSize::_64 => write!(f, "long"),
                }
            }
            Self::Float(FloatSize::LongDouble) => write!(f, "long double"),
            Self::Float(FloatSize::_64) => write!(f, "double"),
            Self::Float(FloatSize::_32) => write!(f, "float"),
            Self::Bool => write!(f, "_Bool"),
            Self::Ptr(pointee) => write!(f, "{pointee:?}*"),
            Self::Struct(name) => write!(f, "struct {name}"),
            Self::Union(name) => write!(f, "union {name}"),
            Self::Enum(name) => write!(f, "enum {name}"),
            Self::Typename(name) => write!(f, "typename {name}"),
            Self::Void => write!(f, "void"),
            Self::Unknown => write!(f, "unknown"),
        }
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StorageSpecifier {
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
    ret_ty: TyExpr,
    args: Vec<TyExpr>,
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

    fn parse_ty_expr(&mut self, prev_span: Span) -> Option<TyExpr> {
        let mut signness_flag = Option::<Signness>::None;
        let mut signness_flag_span = Option::<Span>::None;
        let mut prev_span = prev_span;
        loop {
            let token = self.expect_peek_token(prev_span)?;
            prev_span = token.span();
            match token.inner() {
                Token::Signed => {
                    self.tokens.next();
                    match signness_flag {
                        Some(Signness::Signed) => fixme!("warnings for unneeded `signed` flag"),
                        Some(Signness::Unsigned) => {
                            self.err_reporter
                                .report(&Error::ConflictingSignness.to_spanned(prev_span));
                        }
                        None => signness_flag = Some(Signness::Signed),
                    };
                    signness_flag_span = Some(
                        signness_flag_span.map_or_else(|| prev_span, |span| span.join(prev_span)),
                    )
                }
                Token::Unsigned => {
                    self.tokens.next();
                    match signness_flag {
                        Some(Signness::Signed) => {
                            self.err_reporter
                                .report(&Error::ConflictingSignness.to_spanned(prev_span));
                        }
                        Some(Signness::Unsigned) => fixme!("warnings for unneeded `unsigned` flag"),
                        None => signness_flag = Some(Signness::Unsigned),
                    };
                    signness_flag_span = Some(
                        signness_flag_span.map_or_else(|| prev_span, |span| span.join(prev_span)),
                    )
                }
                Token::Restrict => {
                    self.tokens.next();
                    self.err_reporter
                        .report(&Error::RestrictOnNonPointer.to_spanned(prev_span));
                }
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
                | Token::Ident(..) => break,
                Token::Const => todo!(),
                Token::Volatile => todo!(),
                Token::Complex => todo!(),
                Token::Imaginary => todo!(),
                _ => {
                    self.err_reporter
                        .report(&Error::ExpectTyExpr.to_spanned(prev_span));
                    match signness_flag {
                        Some(sign) => return Some(TyExpr::Int(sign, IntSize::_32)),
                        None => return Some(TyExpr::Unknown),
                    }
                }
            }
        }
        let signness = signness_flag.unwrap_or(Signness::Signed);
        macro report_if_has_signness() {{
            if signness_flag.is_some() {
                self.err_reporter
                    .report(&Error::InvalidSignnessFlag.to_spanned(signness_flag_span.unwrap()));
            }
        }}
        let token = self.expect_peek_token(prev_span)?;
        let span = token.span();
        match token.inner() {
            Token::Char => {
                self.tokens.next();
                Some(TyExpr::Int(signness, IntSize::_8))
            }
            Token::Short => {
                self.tokens.next();
                Some(TyExpr::Int(signness, IntSize::_16))
            }
            Token::Int => {
                self.tokens.next();
                Some(TyExpr::Int(signness, IntSize::_32))
            }
            Token::Long => {
                self.tokens.next();
                next_token_if_matches!(self.tokens, Token::Long);
                Some(TyExpr::Int(signness, IntSize::_64))
            }
            Token::Float => {
                self.tokens.next();
                report_if_has_signness!();
                Some(TyExpr::Float(FloatSize::_32))
            }
            Token::Double => {
                self.tokens.next();
                report_if_has_signness!();
                Some(TyExpr::Float(FloatSize::_64))
            }
            Token::Void => {
                self.tokens.next();
                report_if_has_signness!();
                Some(TyExpr::Void)
            }
            Token::Bool => {
                self.tokens.next();
                report_if_has_signness!();
                Some(TyExpr::Bool)
            }
            Token::Struct => {
                self.tokens.next();
                report_if_has_signness!();
                let ty = expect_ident!(self, span)
                    .map_or_else(|| TyExpr::Unknown, |(name, _)| TyExpr::Struct(name));
                Some(ty)
            }
            Token::Enum => {
                self.tokens.next();
                report_if_has_signness!();
                let ty = expect_ident!(self, span)
                    .map_or_else(|| TyExpr::Unknown, |(name, _)| TyExpr::Enum(name));
                Some(ty)
            }
            Token::Union => {
                self.tokens.next();
                report_if_has_signness!();
                let ty = expect_ident!(self, span)
                    .map_or_else(|| TyExpr::Unknown, |(name, _)| TyExpr::Union(name));
                Some(ty)
            }
            &Token::Ident(name) => match signness_flag {
                Some(sign) => Some(TyExpr::Int(sign, IntSize::_32)),
                _ => {
                    self.tokens.next();
                    Some(TyExpr::Typename(name))
                }
            },
            Token::Signed | Token::Unsigned | Token::Const | Token::Volatile | Token::Restrict => {
                unreachable!()
            }
            _ => match signness_flag {
                Some(s) => Some(TyExpr::Int(s, IntSize::_32)),
                None => {
                    self.err_reporter
                        .report(&Error::ExpectTyExpr.to_spanned(span));
                    Some(TyExpr::Unknown)
                }
            },
        }
    }

    fn parse_decl(
        &mut self,
        storage_spec: StorageSpecifier,
        prev_span: Span,
    ) -> Option<Spanned<Expr>> {
        let ty = self.parse_ty_expr(prev_span)?;
        let ident_token = self.expect_next_token(prev_span)?;
        let ident_token_span = ident_token.span();
        let ident = match_into!(ident_token.into_inner(), Token::Ident(s) => s)
            .ok_or(Error::ExpectIdent.to_spanned(ident_token_span))
            .inspect_err(|e| self.err_reporter.report(e))
            .ok()?;
        match self.tokens.next() {
            Some(t) if t.inner() == &Token::Eq => todo!(),
            Some(t) if t.inner() == &Token::ParenOpen => todo!(),
            _ => Some(
                Expr::VarDecl(storage_spec, ty, ident).to_spanned(prev_span.join(ident_token_span)),
            ),
        }
    }

    fn parse_expr(&mut self, _prec: u8) -> Option<Spanned<Expr>> {
        let token = self.tokens.peek()?;
        let span = token.span();
        macro parse_prefix_op($prec:expr, $opkind:expr $(,)?) {{
            let operand = self
                .parse_expr($prec)
                .ok_or(Error::UnexpectedEof.to_spanned(span))
                .inspect_err(|e| self.err_reporter.report(e))
                .ok()?;
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
                        self.parse_decl(StorageSpecifier::Unspecified, span)
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
            | Token::Imaginary => self.parse_decl(StorageSpecifier::Unspecified, span),
            Token::Auto => {
                self.tokens.next();
                self.parse_decl(StorageSpecifier::Auto, span)
            }
            Token::Register => {
                self.tokens.next();
                self.parse_decl(StorageSpecifier::Register, span)
            }
            Token::Extern => {
                self.tokens.next();
                self.parse_decl(StorageSpecifier::Extern, span)
            }
            Token::Static => {
                self.tokens.next();
                self.parse_decl(StorageSpecifier::Static, span)
            }
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
                let Some((ident, ident_span)) = expect_ident!(self, span) else {
                    return Some(Expr::Error.to_spanned(span));
                };
                Some(Expr::Goto(ident).to_spanned(span.join(ident_span)))
            }
            Token::If => todo!(),
            Token::Inline => todo!(),
            Token::Return => todo!(),
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
                // `&&` can be used to take reference twice.
                // The outer `&` would be invalid (since the value produced by `&expr` is an rvalue),
                // but error would be reported later down the line.
                let inner_operand = self
                    .parse_expr(2)
                    .ok_or(Error::UnexpectedEof.to_spanned(span))
                    .inspect_err(|e| self.err_reporter.report(e))
                    .ok()?;
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
        self.parse_expr(16)
    }
}
