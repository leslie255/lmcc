#![allow(dead_code)]
use std::{iter::Peekable, rc::Rc};

use crate::{
    error::{Error, ErrorReporter, Spanned, ToSpanned},
    intern_str::InternStr,
    token::NumValue,
    utils::match_into,
};

#[allow(unused_imports)]
use super::token::{Token, TokenStream};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
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
    VarDecl(InternStr<'static>, TyExpr),
    VarDeclAssign(TyExpr, Spanned<InternStr<'static>>, Box<Spanned<Expr>>),
    Return(Box<Spanned<Expr>>),
    FnDef(InternStr<'static>, Signature, Option<Vec<Spanned<Expr>>>),
    Labal(InternStr<'static>),
    Goto(InternStr<'static>),
    Break,
    Continue,
}
impl ToSpanned for Expr {}
#[derive(Debug, Clone, PartialEq)]
pub enum TyExpr {
    Int(Signness, IntSize),
    Float(FloatSize),
    Ptr(Box<TyExpr>),
    Struct(InternStr<'static>),
    Union(InternStr<'static>),
    Enum(InternStr<'static>),
    Typename(InternStr<'static>),
    Void,
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

macro next_or_err_eof($token_stream:expr, $prev_span:expr $(,)?) {{
    let prev_span = $prev_span;
    $token_stream
        .next()
        .ok_or(Error::UnexpectedEof.to_spanned((prev_span.file, prev_span.end)))
}}

macro next_token_if($token_stream:expr, $pat:pat $(,)?) {{
    $token_stream.next_if(|t| matches!(t.inner(), $pat))
}}

impl Parser {
    pub fn new(err_reporter: Rc<ErrorReporter>, tokens: Peekable<TokenStream>) -> Self {
        Self {
            err_reporter,
            tokens,
        }
    }

    fn parse_expr(&mut self, prec: u8) -> Option<Spanned<Expr>> {
        let token = self.tokens.next()?;
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
        match token.into_inner() {
            Token::Ident(s) => {
                if next_token_if!(self.tokens, Token::Colon).is_some() {
                    Some(Expr::Labal(s).to_spanned(span))
                } else {
                    Some(Expr::Ident(s).to_spanned(span))
                }
            }
            Token::NumLiteral(n) => Some(Expr::NumLiteral(n).to_spanned(span)),
            Token::StrLiteral(s) => Some(Expr::StrLiteral(s).to_spanned(span)),
            Token::CharLiteral(c) => Some(Expr::CharLiteral(c).to_spanned(span)),
            Token::Auto => todo!(),
            Token::Break => Some(Expr::Break.to_spanned(span)),
            Token::Case => todo!(),
            Token::Char => todo!(),
            Token::Const => todo!(),
            Token::Continue => Some(Expr::Continue.to_spanned(span)),
            Token::Default => todo!(),
            Token::Do => todo!(),
            Token::Double => todo!(),
            Token::Else => todo!(),
            Token::Enum => todo!(),
            Token::Extern => todo!(),
            Token::Float => todo!(),
            Token::For => todo!(),
            Token::Goto => {
                let token = next_or_err_eof!(self.tokens, span)
                    .inspect_err(|e| self.err_reporter.report(e))
                    .ok()?;
                let label_span = token.span();
                let label_ident = match_into!(token.into_inner(), Token::Ident(s) => s)
                    .ok_or(Error::ExpectIdent.to_spanned(label_span))
                    .inspect_err(|e| self.err_reporter.report(e))
                    .ok()?;
                Some(Expr::Goto(label_ident).to_spanned(label_span.join(label_span)))
            }
            Token::If => todo!(),
            Token::Inline => todo!(),
            Token::Int => todo!(),
            Token::Long => todo!(),
            Token::Register => todo!(),
            Token::Restrict => todo!(),
            Token::Return => todo!(),
            Token::Short => todo!(),
            Token::Signed => todo!(),
            Token::Sizeof => todo!(),
            Token::Static => todo!(),
            Token::Struct => todo!(),
            Token::Switch => todo!(),
            Token::Typedef => todo!(),
            Token::Union => todo!(),
            Token::Unsigned => todo!(),
            Token::Void => todo!(),
            Token::Volatile => todo!(),
            Token::While => todo!(),
            Token::Bool => todo!(),
            Token::Complex => todo!(),
            Token::Imaginary => todo!(),
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
