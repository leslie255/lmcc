use std::{collections::HashMap, fmt::Debug, iter::Peekable, rc::Rc};

use crate::{
    ast::*,
    error::{Error, ErrorReporter, Span, Spanned, ToSpanned},
    token::NumValue,
    utils::{fixme, match_into_unchecked, IdentStr},
};

use super::token::{Token, TokenStream};

// Cases unable to handle:
// - function pointer types

#[derive(Debug, Clone)]
pub struct Parser {
    err_reporter: Rc<ErrorReporter>,
    tokens: Peekable<TokenStream>,
    typenames: Vec<HashMap<IdentStr, Ty>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Default)]
struct DeclSpecifiers {
    // --- storage-class (linkage-class) ---
    static_: Option<Span>,
    extern_: Option<Span>,
    register: Option<Span>,
    auto: Option<Span>,
    typedef: Option<Span>,

    // --- inline ---
    inline: Option<Span>,

    // --- type specifiers ---
    signness: Option<Spanned<Signness>>,
    char: Option<Span>,
    short: Option<Span>,
    int: Option<Span>,
    long0: Option<Span>,
    long1: Option<Span>,
    float: Option<Span>,
    double: Option<Span>,
    bool: Option<Span>,

    // --- type name ---
    typename: Option<Spanned<IdentStr>>,

    // --- type qualifiers ---
    const_: Option<Span>,
    restrict: Option<Span>,
    volatile: Option<Span>,
}

impl DeclSpecifiers {
    /// Returns `Ok(())` if the added token is a decl specifier.
    /// Returns `Err(())` if the added token is not a decl specifier.
    /// If the added token **is** a decl specifier but there is an error (e.g. conflicting signness), reports error to `err_reporter` and returns `Ok(())`.
    /// Use `add_typename` for adding a typename.
    fn add_token(
        &mut self,
        err_reporter: &ErrorReporter,
        token: &Spanned<Token>,
    ) -> Result<(), ()> {
        let span = token.span();
        macro set_flag($flag:tt $(,)?) {{
            match self.$flag {
                Some(_) => {
                    err_reporter.report(&Error::DuplicateSpecifier.to_spanned(span));
                }
                None => {
                    self.$flag = Some(span);
                }
            }
        }}
        match token.inner() {
            Token::Complex => {
                err_reporter
                    .report(&Error::Unsupported("_Complex is not supported").to_spanned(span));
            }
            Token::Imaginary => {
                err_reporter
                    .report(&Error::Unsupported("_Imaginary is not supported").to_spanned(span));
            }
            Token::Static => set_flag!(static_),
            Token::Extern => set_flag!(extern_),
            Token::Register => set_flag!(register),
            Token::Auto => set_flag!(auto),
            Token::Typedef => set_flag!(typedef),
            Token::Inline => set_flag!(inline),
            Token::Char => set_flag!(char),
            Token::Short => set_flag!(short),
            Token::Int => set_flag!(int),
            Token::Float => set_flag!(float),
            Token::Double => set_flag!(double),
            Token::Bool => set_flag!(bool),
            Token::Const => set_flag!(const_),
            Token::Restrict => set_flag!(restrict),
            Token::Volatile => set_flag!(volatile),
            Token::Long => match self.long0 {
                None => {
                    self.long0 = Some(span);
                }
                Some(_) => match self.long1 {
                    Some(_) => {
                        err_reporter.report(&Error::LongLongLongIsTooLong.to_spanned(span));
                    }
                    None => {
                        self.long1 = Some(span);
                    }
                },
            },
            Token::Signed => match self.signness {
                Some(s) => {
                    let span = s.span();
                    if s.into_inner() == Signness::Unsigned {
                        err_reporter.report(&Error::ConflictingSignness.to_spanned(span));
                    } else {
                        fixme!("warning for duplicated `signed`")
                    }
                }
                None => {
                    self.signness = Some(Signness::Signed.to_spanned(span));
                }
            },
            Token::Unsigned => match self.signness {
                Some(s) => {
                    let span = s.span();
                    if s.into_inner() == Signness::Unsigned {
                        err_reporter.report(&Error::ConflictingSignness.to_spanned(span));
                    } else {
                        fixme!("warning for duplicated `signed`")
                    }
                }
                None => {
                    self.signness = Some(Signness::Signed.to_spanned(span));
                }
            },
            _ => return Err(()),
        }
        Ok(())
    }
    /// Ident must be made sure to be a typename from context, otherwise it should be treated as a normal ident expression, the ambiguous cases (e.g. `a * b`) would be dealt with later down the line.
    fn add_typename(&mut self, typename: Spanned<IdentStr>) {
        match self.typename {
            Some(name) => panic!("calling `add_typename` multiple times on one `DeclSpecifier` (existing typename {:?} @ {:?}", name, name.span()),
            None => self.typename = Some(typename),
        }
    }
    /// For invalid combinations of specifiers, report the error and return a guess of what the user meant.
    fn try_into_var_speci(&self, err_reporter: &ErrorReporter) -> VarSpecifier {
        macro ensure_no($keyword:tt, $err:tt $(,)?) {
            self.$keyword
                .inspect(|&span| err_reporter.report(&Error::$err.to_spanned(span)))
        }
        ensure_no!(inline, InlineInVarDecl);
        if self.static_.is_some() {
            ensure_no!(register, ConflictingStorageClass);
            ensure_no!(extern_, ConflictingStorageClass);
            ensure_no!(auto, ConflictingStorageClass);
            ensure_no!(typedef, ConflictingStorageClass);
            VarSpecifier::Static
        } else if self.register.is_some() {
            ensure_no!(extern_, ConflictingStorageClass);
            ensure_no!(auto, ConflictingStorageClass);
            ensure_no!(typedef, ConflictingStorageClass);
            VarSpecifier::Register
        } else if self.extern_.is_some() {
            ensure_no!(register, ConflictingStorageClass);
            ensure_no!(auto, ConflictingStorageClass);
            ensure_no!(typedef, ConflictingStorageClass);
            VarSpecifier::Extern
        } else if self.auto.is_some() {
            ensure_no!(typedef, ConflictingStorageClass);
            VarSpecifier::Auto
        } else if self.typedef.is_some() {
            VarSpecifier::Typedef
        } else {
            VarSpecifier::Unspecified
        }
    }
    /// For invalid combinations of specifiers, report the error and return a guess of what the user meant.
    fn try_into_func_speci(&self, err_reporter: &ErrorReporter) -> FuncSpecifier {
        macro ensure_no($keyword:tt, $err:tt $(,)?) {
            self.$keyword
                .inspect(|&span| err_reporter.report(&Error::$err.to_spanned(span)))
        }
        ensure_no!(register, InvalidStorageClass);
        ensure_no!(auto, InvalidStorageClass);
        let linkage = if self.extern_.is_some() {
            ensure_no!(static_, ConflictingStorageClass);
            LinkageSpecifier::Extern
        } else if self.static_.is_some() {
            ensure_no!(extern_, ConflictingStorageClass);
            LinkageSpecifier::Static
        } else {
            LinkageSpecifier::Default
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
            typenames: vec![HashMap::new()],
        }
    }

    fn is_typename(&self, ident: IdentStr) -> bool {
        self.typenames
            .iter()
            .rev()
            .find(|m| m.contains_key(&ident))
            .is_some()
    }

    fn enters_block(&mut self) {
        self.typenames.push(HashMap::new());
    }

    /// Crashes if `self.typenames` is empty.
    fn leaves_block(&mut self) {
        self.typenames
            .pop()
            .expect("Parser::leaves_block called when typename stack is empty");
    }

    /// Crashes if `self.typenames` is empty.
    fn add_typename(&mut self, typename: IdentStr, ty: Ty) {
        self.typenames
            .last_mut()
            .expect("Parser::add_typename called when typename stack is empty")
            .insert(typename, ty);
    }

    /// If token stream ends, reports `UnexpectedEOF` and return `None`.
    fn expect_next_token(&mut self, prev_span: Span) -> Option<Spanned<Token>> {
        self.tokens
            .next()
            .ok_or(Error::UnexpectedEof.to_spanned((prev_span.file, prev_span.end)))
            .inspect_err(|e| self.err_reporter.report(e))
            .ok()
    }

    fn expect_ident(&mut self, prev_span: Span) -> Option<Spanned<IdentStr>> {
        let token = self.expect_next_token(prev_span)?;
        let span = token.span();
        if let Token::Ident(ident) = token.into_inner() {
            Some(ident.to_spanned(prev_span))
        } else {
            self.err_reporter
                .report(&Error::ExpectIdent.to_spanned(span));
            None
        }
    }

    /// If token stream ends, reports `UnexpectedEOF` and return `None`.
    fn expect_peek_token(&mut self, prev_span: Span) -> Option<&Spanned<Token>> {
        self.tokens
            .peek()
            .ok_or(Error::UnexpectedEof.to_spanned((prev_span.file, prev_span.end)))
            .inspect_err(|e| self.err_reporter.report(e))
            .ok()
    }

    /// Parse decl specifiers.
    /// Sometimes after parsing the decl specifiers the next token is also taken.
    #[must_use]
    fn parse_decl_speci(
        &mut self,
        prev_span: Span,
        mut decl_speci: DeclSpecifiers,
    ) -> Spanned<DeclSpecifiers> {
        let mut end_span = prev_span;
        while let Some(token) = self.tokens.peek() {
            if decl_speci.add_token(&self.err_reporter, token).is_ok() {
                end_span = token.span();
                self.tokens.next();
            } else if let &Token::Ident(s) = token.inner() {
                let span = token.span();
                if self.is_typename(s) {
                    self.tokens.next();
                    decl_speci.add_typename(s.to_spanned(span));
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        decl_speci.to_spanned(prev_span.join(end_span))
    }

    /// Deduce a type from decl specifiers.
    fn deduce_ty_speci(&mut self, decl_speci: &Spanned<DeclSpecifiers>) -> Spanned<Ty> {
        macro signness() {
            decl_speci
                .signness
                .map(|s| s.into_inner())
                .unwrap_or(Signness::Signed)
        }
        macro ensure_no_signness() {
            if let Some(signness) = decl_speci.signness {
                self.err_reporter
                    .report(&Error::InvalidSignnessFlag.to_spanned(signness.span()));
            }
        }
        macro ensure_no($speci:tt, $err:tt $(,)?) {
            if let Some(span) = decl_speci.$speci {
                self.err_reporter.report(&Error::$err.to_spanned(span));
            }
        }
        // TODO: more accurate span.
        if decl_speci.char.is_some() {
            ensure_no!(short, ConflictingTypeSpecifier);
            ensure_no!(int, ConflictingTypeSpecifier);
            ensure_no!(short, ConflictingTypeSpecifier);
            ensure_no!(long0, ConflictingTypeSpecifier);
            ensure_no!(float, ConflictingTypeSpecifier);
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            TyKind::Int(signness!(), IntSize::_8)
        } else if decl_speci.short.is_some() {
            ensure_no!(int, ConflictingTypeSpecifier);
            ensure_no!(long0, ConflictingTypeSpecifier);
            ensure_no!(float, ConflictingTypeSpecifier);
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            TyKind::Int(signness!(), IntSize::_16)
        } else if decl_speci.int.is_some() {
            ensure_no!(long0, ConflictingTypeSpecifier);
            ensure_no!(float, ConflictingTypeSpecifier);
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            TyKind::Int(signness!(), IntSize::_32)
        } else if decl_speci.long0.is_some() {
            ensure_no!(float, ConflictingTypeSpecifier);
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            TyKind::Int(signness!(), IntSize::_64)
        } else if decl_speci.float.is_some() {
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            ensure_no_signness!();
            TyKind::Float(FloatSize::_32)
        } else if decl_speci.double.is_some() {
            ensure_no_signness!();
            ensure_no!(bool, ConflictingTypeSpecifier);
            TyKind::Float(FloatSize::_64)
        } else if decl_speci.bool.is_some() {
            ensure_no_signness!();
            TyKind::Bool
        } else if let Some(signness) = decl_speci.signness {
            TyKind::Int(signness.into_inner(), IntSize::_32)
        } else if let Some(typename) = decl_speci.typename {
            TyKind::Typename(*typename)
        } else {
            self.err_reporter
                .report(&Error::ExpectTy.to_spanned(decl_speci.span()));
            TyKind::Error
        }
        .to_ty(decl_speci.const_.is_some(), decl_speci.volatile.is_some())
        .to_spanned(decl_speci.span())
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
    fn parse_ptr_decl(&mut self, mut ty: Spanned<Ty>) -> Spanned<Ty> {
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
        ty
    }

    fn parse_fn_decl_args(&mut self, prev_span: Span) -> Option<Vec<(Ty, Option<IdentStr>)>> {
        let mut args = Vec::<(Ty, Option<IdentStr>)>::new();
        let token = self.expect_peek_token(prev_span)?;
        if token.inner() == &Token::ParenClose {
            self.tokens.next();
        } else {
            loop {
                let mut prev_span = prev_span;
                let decl_speci = DeclSpecifiers::default();
                let decl_speci = self.parse_decl_speci(prev_span, decl_speci);
                {
                    macro ensure_no($keyword:tt, $err:tt $(,)?) {
                        decl_speci.$keyword.inspect(|&span| {
                            self.err_reporter.report(&Error::$err.to_spanned(span))
                        })
                    }
                    ensure_no!(static_, InvalidStorageClass);
                    ensure_no!(extern_, InvalidStorageClass);
                    ensure_no!(auto, InvalidStorageClass);
                    ensure_no!(inline, InlineInVarDecl);
                }
                let ty = self.deduce_ty_speci(&decl_speci);
                let mut ty = self.parse_ptr_decl(ty);
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
        Some(args)
    }

    /// Parse the content after declaration specifiers.
    fn parse_decl_content(
        &mut self,
        prev_span: Span,
        decl_speci: &Spanned<DeclSpecifiers>,
    ) -> Option<Spanned<Expr>> {
        let ty = self.deduce_ty_speci(decl_speci);
        let ty = self.parse_ptr_decl(ty);
        let ty_span = ty.span();
        let (ident, ident_span) = self.expect_ident(ty_span)?.into_pair();
        let ty = self.parse_arr_decl(ty)?;

        match self.tokens.peek() {
            Some(t) if t.inner() == &Token::Comma => todo!("list decls"),
            Some(t) if t.inner() == &Token::Eq => {
                self.tokens.next();
                let var_specs = decl_speci.try_into_var_speci(&self.err_reporter);
                let rhs = self.parse_expr(ident_span, 16)?;
                let rhs_span = rhs.span();
                Some(
                    Expr::Decl(DeclItem::Var(var_specs, ty, ident, Some(Box::new(rhs))))
                        .to_spanned(ty_span.join(rhs_span)),
                )
            }
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
                let token = self.tokens.peek();
                let func_speci = decl_speci.try_into_func_speci(&self.err_reporter);
                let sig = Signature { ret_ty, args };
                match token.map(Spanned::as_pair) {
                    Some((Token::BraceOpen, span)) => {
                        self.tokens.next();
                        let (body, span) = self.parse_block(span)?.into_pair();
                        Some(
                            Expr::Decl(DeclItem::Func(func_speci, sig, ident, Some(body)))
                                .to_spanned(ty_span.join(span)),
                        )
                    }
                    Some((Token::Comma, _)) => todo!("list decls"),
                    Some(..) | None => Some(
                        Expr::Decl(DeclItem::Func(func_speci, sig, ident, None))
                            .to_spanned(ty_span.join(prev_span)),
                    ),
                }
            }
            _ => {
                let var_specs = decl_speci.try_into_var_speci(&self.err_reporter);
                Some(
                    Expr::Decl(DeclItem::Var(var_specs, ty, ident, None))
                        .to_spanned(ty_span.join(ident_span)),
                )
            }
        }
    }

    /// Parse an entire decl statement.
    /// Unlike most `parse_` functions, this function does not start with the first token already consumed.
    fn parse_decl(&mut self, prev_span: Span) -> Option<Spanned<Expr>> {
        let decl_speci = DeclSpecifiers::default();
        let decl_speci = self.parse_decl_speci(prev_span, decl_speci);
        self.parse_decl_content(prev_span, &decl_speci)
    }

    /// Start with `{` token already consumed.
    fn parse_block(&mut self, mut prev_span: Span) -> Option<Spanned<Vec<Spanned<Expr>>>> {
        self.enters_block();
        let start_span = prev_span;
        let mut body = Vec::<Spanned<Expr>>::new();
        loop {
            let (token, span) = self.expect_peek_token(prev_span)?.as_pair();
            prev_span = span;
            match token {
                Token::BraceClose => {
                    self.tokens.next();
                    break;
                }
                _ => {
                    let expr = self.parse_expr(prev_span, 16)?;
                    let omits_semicolon = expr.omits_semicolon();
                    prev_span = expr.span();
                    body.push(expr);
                    let token = self.tokens.peek();
                    if token.is_some_and(|t| t.inner() == &Token::Semicolon) {
                        self.tokens.next();
                    } else {
                        if !omits_semicolon {
                            self.err_reporter
                                .report(&Error::MissingSemicolon.to_spanned(span.tail()));
                        }
                    }
                }
            }
        }
        self.leaves_block();
        Some(body.to_spanned(start_span.join(prev_span)))
    }

    /// Start with opening paren already consumed.
    fn parse_paren(&mut self, prev_span: Span) -> Option<Spanned<Expr>> {
        macro expect_paren_close($prev_span:expr) {
            match self.tokens.peek().map(Spanned::as_pair) {
                Some((Token::ParenClose, span)) => {
                    self.tokens.next();
                    span
                }
                Some((_, span)) => {
                    self.err_reporter
                        .report(&Error::ExpectToken(Token::ParenClose).to_spanned(span));
                    return Some(Expr::Error.to_spanned(prev_span.join($prev_span)));
                }
                None => {
                    self.err_reporter.report(
                        &Error::ExpectToken(Token::ParenClose).to_spanned($prev_span.tail()),
                    );
                    return Some(Expr::Error.to_spanned(prev_span.join($prev_span)));
                }
            }
        }
        let (token, span) = self.expect_peek_token(prev_span)?.as_pair();
        match token {
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
            | Token::Imaginary => {
                let decl_speci = DeclSpecifiers::default();
                let decl_speci = self.parse_decl_speci(prev_span, decl_speci);
                let ty = self.deduce_ty_speci(&decl_speci);
                let closing_paren_span = expect_paren_close!(decl_speci.span());
                let expr = self.parse_expr(closing_paren_span, 2)?;
                let span = prev_span.join(expr.span());
                Some(Expr::Typecast(ty.into_inner(), Box::new(expr)).to_spanned(span))
            }
            &Token::Ident(s) => {
                if self.is_typename(s) {
                    self.tokens.next();
                    let mut decl_speci = DeclSpecifiers::default();
                    decl_speci.add_typename(s.to_spanned(span));
                    let decl_speci = self.parse_decl_speci(prev_span, decl_speci);
                    let ty = self.deduce_ty_speci(&decl_speci);
                    let closing_paren_span = expect_paren_close!(decl_speci.span());
                    let expr = self.parse_expr(closing_paren_span, 2)?;
                    let span = prev_span.join(expr.span());
                    Some(Expr::Typecast(ty.into_inner(), Box::new(expr)).to_spanned(span))
                } else {
                    let expr = self.parse_expr(span, 16)?;
                    expect_paren_close!(expr.span());
                    // FIXME: ensure it's not a statement.
                    Some(expr)
                }
            }
            _ => {
                let expr = self.parse_expr(span, 16)?;
                expect_paren_close!(expr.span());
                // FIXME: ensure it's not a statement.
                Some(expr)
            }
        }
    }

    /// Start with `(` already consumed.
    fn parse_call(&mut self, mut prev_span: Span, callee: Spanned<Expr>) -> Option<Spanned<Expr>> {
        let opening_paren_span = prev_span;
        let mut args = Vec::<Spanned<Expr>>::new();
        if self.expect_peek_token(prev_span)?.inner() != &Token::ParenClose {
            loop {
                let expr = self.parse_expr(prev_span, 15)?;
                prev_span = expr.span();
                args.push(expr);
                let (token, span) = self.expect_peek_token(prev_span)?.as_pair();
                prev_span = span;
                match token {
                    Token::ParenClose => {
                        self.tokens.next();
                        break;
                    }
                    Token::Comma => {
                        self.tokens.next();
                        continue;
                    }
                    _ => {
                        self.err_reporter.report(
                            &Error::ExpectTokens(&[Token::Comma, Token::ParenClose])
                                .to_spanned(span),
                        );
                        break;
                    }
                }
            }
        } else {
            self.tokens.next();
        }
        let args = args.to_spanned(opening_paren_span.join(prev_span));
        let span = callee.span().join(prev_span);
        Some(Expr::Call(Box::new(callee), args).to_spanned(span))
    }

    fn parse_expr(&mut self, prev_span: Span, prec: u16) -> Option<Spanned<Expr>> {
        let token = self.expect_peek_token(prev_span)?;
        let span = token.span();
        macro parse_prefix_op($prec:expr, $opkind:expr $(,)?) {{
            self.tokens.next();
            let operand = self.parse_expr(span, $prec)?;
            let operand_span = operand.span();
            Some(Expr::PrefixOp($opkind, Box::new(operand)).to_spanned(span.join(operand_span)))
        }}
        let expr = match token.inner() {
            &Token::Ident(s) => {
                self.tokens.next();
                if self.is_typename(s) {
                    let mut decl_speci = DeclSpecifiers::default();
                    decl_speci.add_typename(s.to_spanned(span));
                    let decl_speci = self.parse_decl_speci(span, decl_speci);
                    let expr = self.parse_decl_content(decl_speci.span(), &decl_speci);
                    expr.as_ref().inspect(|expr| match expr.inner() {
                        Expr::Decl(DeclItem::Var(VarSpecifier::Typedef, ty, name, _)) => {
                            self.add_typename(*name, ty.inner().clone());
                        }
                        _ => (),
                    });
                    expr
                } else if self
                    .tokens
                    .peek()
                    .is_some_and(|t| t.inner() == &Token::Colon)
                {
                    self.tokens.next();
                    Some(Expr::Labal(s).to_spanned(span))
                } else {
                    Some(Expr::Ident(s).to_spanned(span))
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
            | Token::Inline
            | Token::Typedef => {
                let expr = self.parse_decl(span);
                expr.as_ref().inspect(|expr| match expr.inner() {
                    Expr::Decl(DeclItem::Var(VarSpecifier::Typedef, ty, name, _)) => {
                        self.add_typename(*name, ty.inner().clone());
                    }
                    _ => (),
                });
                expr
            }
            Token::Break => {
                self.tokens.next();
                Some(Expr::Break.to_spanned(span))
            }
            Token::Continue => {
                self.tokens.next();
                Some(Expr::Continue.to_spanned(span))
            }
            Token::Do => todo!("do-while loops"),
            Token::For => todo!("for loops"),
            Token::Goto => {
                self.tokens.next();
                if let Some((ident, ident_span)) = self.expect_ident(span).map(Spanned::into_pair) {
                    Some(Expr::Goto(ident).to_spanned(span.join(ident_span)))
                } else {
                    return Some(Expr::Error.to_spanned(span));
                }
            }
            Token::If => todo!("if"),
            Token::Return => {
                self.tokens.next();
                match self.tokens.peek() {
                    Some(t) if matches!(t.inner(), Token::Semicolon | Token::Comma) => {
                        Some(Expr::Return(None).to_spanned(span))
                    }
                    None => Some(Expr::Return(None).to_spanned(span)),
                    Some(..) => {
                        let operand = self.parse_expr(span, 16)?;
                        let operand_span = operand.span();
                        Some(
                            Expr::Return(Some(Box::new(operand)))
                                .to_spanned(span.join(operand_span)),
                        )
                    }
                }
            }
            Token::Sizeof => todo!("sizeof"),
            Token::Switch => todo!("switch"),
            Token::While => todo!("while"),
            Token::Add => parse_prefix_op!(2, PrefixOpKind::UnaryAdd),
            Token::AddAdd => parse_prefix_op!(2, PrefixOpKind::PreInc),
            Token::Sub => parse_prefix_op!(2, PrefixOpKind::UnarySub),
            Token::SubSub => parse_prefix_op!(2, PrefixOpKind::PreDec),
            Token::Mul => parse_prefix_op!(2, PrefixOpKind::Deref),
            Token::Tilde => parse_prefix_op!(2, PrefixOpKind::BitNot),
            Token::Exc => parse_prefix_op!(2, PrefixOpKind::Not),
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
            Token::ParenOpen => {
                self.tokens.next();
                self.parse_paren(span)
            }
            Token::Case => {
                self.tokens.next();
                let expr = self.parse_expr(span, 16)?;
                let end_span = match self.expect_peek_token(expr.span())?.as_pair() {
                    (Token::Colon, span) => {
                        self.tokens.next();
                        span
                    }
                    (_, span) => {
                        self.err_reporter
                            .report(&Error::ExpectToken(Token::Colon).to_spanned(span));
                        expr.span()
                    }
                };
                Some(Expr::Case(Box::new(expr)).to_spanned(span.join(end_span)))
            }
            Token::Default => {
                self.tokens.next();
                let end_span = match self.expect_peek_token(span)?.as_pair() {
                    (Token::Colon, span) => {
                        self.tokens.next();
                        span
                    }
                    (_, span) => {
                        self.err_reporter
                            .report(&Error::ExpectToken(Token::Colon).to_spanned(span));
                        span
                    }
                };
                Some(Expr::Default.to_spanned(span.join(end_span)))
            }
            Token::BraceOpen => todo!("list/struct literal"),
            Token::Else
            | Token::AddEq
            | Token::SubEq
            | Token::Arrow
            | Token::MulEq
            | Token::Div
            | Token::DivEq
            | Token::Rem
            | Token::RemEq
            | Token::Lt
            | Token::LtLt
            | Token::LtLtEq
            | Token::LtEq
            | Token::Gt
            | Token::GtGt
            | Token::GtGtEq
            | Token::GtEq
            | Token::ExcEq
            | Token::AndEq
            | Token::Eq
            | Token::EqEq
            | Token::Or
            | Token::OrOr
            | Token::OrEq
            | Token::Xor
            | Token::XorEq
            | Token::Backslash
            | Token::Dot
            | Token::Comma
            | Token::Colon
            | Token::Semicolon
            | Token::Ques
            | Token::ParenClose
            | Token::BracketOpen
            | Token::BracketClose
            | Token::BraceClose => {
                let (token, span) = unsafe { self.tokens.next().unwrap_unchecked().into_pair() };
                self.err_reporter
                    .report(&Error::UnexpectedToken(token).to_spanned(span));
                return Some(Expr::Error.to_spanned(span));
            }
        };
        let mut expr = expr?;
        while let Some((token, span)) = self.tokens.peek().map(Spanned::as_pair) {
            macro infix_op($exprkind:tt, $kind:expr, $prec:expr) {{
                if prec <= $prec {
                    break;
                }
                self.tokens.next();
                let rhs = self
                    .parse_expr(span, $prec)
                    .unwrap_or_else(|| Expr::Error.to_spanned(span));
                let span = expr.span().join(rhs.span());
                expr = Expr::$exprkind(Box::new(expr), $kind, Box::new(rhs)).to_spanned(span);
            }}
            macro postfix_op($kind:expr) {{
                // The only two cases are postfix++ and postfix--, both having precedence of 1,
                // so no need for precedence checking.
                self.tokens.next();
                let span = expr.span().join(span);
                expr = Expr::PostfixOp(Box::new(expr), $kind).to_spanned(span);
            }}
            match token {
                // Precedence 1. No need for precedence checking here.
                Token::ParenOpen => {
                    self.tokens.next();
                    expr = self.parse_call(span, expr)?;
                }
                Token::BracketOpen => {
                    self.tokens.next();
                    let operand = self.parse_expr(span, 16)?;
                    let end_span = match self.expect_peek_token(operand.span())?.as_pair() {
                        (Token::BracketClose, span) => {
                            self.tokens.next();
                            span
                        }
                        (_, span) => {
                            self.err_reporter
                                .report(&Error::ExpectToken(Token::BracketClose).to_spanned(span));
                            operand.span()
                        }
                    };
                    let span = expr.span().join(end_span);
                    expr = Expr::Subscript(Box::new(expr), Box::new(operand)).to_spanned(span);
                }
                Token::Dot => todo!("member access"),
                Token::Arrow => todo!("indirect member access"),
                Token::AddAdd => postfix_op!(PostfixOpKind::PostInc),
                Token::SubSub => postfix_op!(PostfixOpKind::PostDec),
                Token::Mul => infix_op!(InfixOp, InfixOpKind::Mul, 3),
                Token::Div => infix_op!(InfixOp, InfixOpKind::Div, 3),
                Token::Rem => infix_op!(InfixOp, InfixOpKind::Rem, 3),
                Token::Add => infix_op!(InfixOp, InfixOpKind::Add, 4),
                Token::Sub => infix_op!(InfixOp, InfixOpKind::Sub, 4),
                Token::LtLt => infix_op!(InfixOp, InfixOpKind::Bsl, 5),
                Token::GtGt => infix_op!(InfixOp, InfixOpKind::Bsr, 5),
                Token::Lt => infix_op!(InfixOp, InfixOpKind::Lt, 6),
                Token::Gt => infix_op!(InfixOp, InfixOpKind::Gt, 6),
                Token::LtEq => infix_op!(InfixOp, InfixOpKind::Le, 6),
                Token::GtEq => infix_op!(InfixOp, InfixOpKind::Ge, 6),
                Token::EqEq => infix_op!(InfixOp, InfixOpKind::Eq, 7),
                Token::ExcEq => infix_op!(InfixOp, InfixOpKind::Ne, 7),
                Token::And => infix_op!(InfixOp, InfixOpKind::BitAnd, 8),
                Token::Xor => infix_op!(InfixOp, InfixOpKind::BitXor, 9),
                Token::Or => infix_op!(InfixOp, InfixOpKind::BitOr, 10),
                Token::AndAnd => infix_op!(InfixOp, InfixOpKind::And, 11),
                Token::OrOr => infix_op!(InfixOp, InfixOpKind::Or, 12),
                Token::Ques => todo!("tenary operator ?:"),
                // Assignments technically have right-to-left associativity, but most compilers don't care,
                // as only expressions with very high (aka. small) precedence can legally be the LHS of assignments.
                Token::Eq => infix_op!(InfixOp, InfixOpKind::Eq, 14),
                Token::AddEq => infix_op!(OpAssign, AssignOpKind::Add, 14),
                Token::SubEq => infix_op!(OpAssign, AssignOpKind::Sub, 14),
                Token::MulEq => infix_op!(OpAssign, AssignOpKind::Mul, 14),
                Token::DivEq => infix_op!(OpAssign, AssignOpKind::Div, 14),
                Token::RemEq => infix_op!(OpAssign, AssignOpKind::Rem, 14),
                Token::LtLtEq => infix_op!(OpAssign, AssignOpKind::Bsl, 14),
                Token::GtGtEq => infix_op!(OpAssign, AssignOpKind::Bsr, 14),
                Token::AndEq => infix_op!(OpAssign, AssignOpKind::BitAnd, 14),
                Token::XorEq => infix_op!(OpAssign, AssignOpKind::BitXor, 14),
                Token::OrEq => infix_op!(OpAssign, AssignOpKind::BitOr, 14),
                // For function calls, we simply specify precedence to be 15 when parsing arguments to a call.
                Token::Comma => infix_op!(InfixOp, InfixOpKind::Comma, 15),
                _ => break,
            }
        }
        Some(expr)
    }
}

impl Iterator for Parser {
    type Item = Spanned<Expr>;

    fn next(&mut self) -> Option<Self::Item> {
        let span = self.tokens.peek()?.span();
        let expr = self.parse_expr(span, 16)?;
        let span = expr.span();
        let token = self.tokens.peek();
        if token.is_some_and(|t| t.inner() == &Token::Semicolon) {
            self.tokens.next();
        } else {
            if !expr.omits_semicolon() {
                self.err_reporter
                    .report(&Error::MissingSemicolon.to_spanned(span.tail()));
            }
        }
        Some(expr)
    }
}
