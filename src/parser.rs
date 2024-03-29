use std::{collections::HashMap, fmt::Debug, iter::Peekable, rc::Rc, vec};

use crate::{
    ast::*,
    error::{Error, ErrorReporter, Span, Spanned, ToSpanned},
    intern_string,
    token::NumValue,
    utils::{fixme, match_into, match_into_unchecked, IdentStr},
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

#[derive(Debug, Clone, PartialEq, Default)]
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
    void: Option<Span>,
    signness: Option<Spanned<Signness>>,
    char: Option<Span>,
    short: Option<Span>,
    int: Option<Span>,
    long0: Option<Span>,
    long1: Option<Span>,
    float: Option<Span>,
    double: Option<Span>,
    bool: Option<Span>,
    struct_: Option<Spanned<(Option<IdentStr>, Option<StructFields>)>>,
    union_: Option<Spanned<(Option<IdentStr>, Option<StructFields>)>>,
    enum_: Option<Spanned<(Option<IdentStr>, Option<EnumFields>)>>,

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
    /// Use `add_struct`, `add_union`, `add_enum` for adding a struct, union, enum.
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
            Token::Void => set_flag!(void),
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
                    self.signness = Some(Signness::Unsigned.to_spanned(span));
                }
            },
            _ => return Err(()),
        }
        Ok(())
    }
    /// Ident must be made sure to be a typename from context, otherwise it should be treated as a normal ident expression, the ambiguous cases (e.g. `a * b`) would be dealt with later down the line.
    fn add_typename(&mut self, typename: Spanned<IdentStr>) {
        match self.typename {
            None => self.typename = Some(typename),
            Some(name) => panic!("calling `add_typename` multiple times on one `DeclSpecifier` (existing typename {:?} @ {:?})", name, name.span()),
        }
    }
    fn add_struct(&mut self, struct_: Spanned<(Option<IdentStr>, Option<StructFields>)>) {
        match &self.struct_ {
            None => self.struct_ = Some(struct_),
            Some(struct_) => panic!("calling `add_struct` multiple times on one `DeclSpecifier` (existing: {:?} @ {:?})", struct_, struct_.span()),
        }
    }
    fn add_union(&mut self, union_: Spanned<(Option<IdentStr>, Option<StructFields>)>) {
        match &self.union_ {
            None => self.union_ = Some(union_),
            Some(union_) => panic!(
                "calling `add_union` multiple times on one `DeclSpecifier` (existing: {:?} @ {:?})",
                union_,
                union_.span()
            ),
        }
    }
    fn add_enum(&mut self, enum_: Spanned<(Option<IdentStr>, Option<EnumFields>)>) {
        match &self.enum_ {
            None => self.enum_ = Some(enum_),
            Some(enum_) => panic!(
                "calling `add_enum` multiple times on one `DeclSpecifier` (existing: {:?} @ {:?})",
                enum_,
                enum_.span()
            ),
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

/// Returns the span.
/// The default behavior on unexpected token is report error and `return None`,
/// but this could be customized using `else => expr`, to replace the `return None` part.
macro expect_token {
    ($self:expr, $token:path, $prev_span:expr, else => $else:expr $(,)?) => {
        match $self.expect_peek_token($prev_span)?.as_pair() {
            ($token, span) => {
                $self.tokens.next();
                span
            }
            (_, span) => {
                $self
                    .err_reporter
                    .report(&Error::ExpectToken($token).to_spanned(span));
                $else
            }
        }
    },
    ($self:expr, $token:path, $prev_span:expr $(,)?) => {{
        let prev_span = $prev_span; // prevent twice-evaluation
        expect_token!($self, $token, prev_span, else => return None)
    }},
}

impl Parser {
    pub fn new(err_reporter: Rc<ErrorReporter>, tokens: Peekable<TokenStream>) -> Self {
        let mut self_ = Self {
            err_reporter,
            tokens,
            typenames: vec![HashMap::new()],
        };
        self_.consume_semicolons(Span::default(), true);
        self_
    }

    fn is_typename(&self, ident: IdentStr) -> bool {
        self.typenames
            .iter()
            .rev()
            .find(|m| m.contains_key(&ident))
            .is_some()
    }

    fn ty_of_typename(&self, typename: IdentStr) -> Option<&Ty> {
        self.typenames.iter().rev().find_map(|m| m.get(&typename))
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

    fn expect_ident(&mut self, prev_span: Span) -> Option<Spanned<IdentStr>> {
        let token = self.expect_peek_token(prev_span)?;
        let span = token.span();
        if let &Token::Ident(ident) = token.inner() {
            self.tokens.next();
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
    fn parse_decl_speci(
        &mut self,
        prev_span: Span,
        mut decl_speci: DeclSpecifiers,
    ) -> Option<Spanned<DeclSpecifiers>> {
        let mut end_span = prev_span;
        macro parse_struct_union($start_span:expr) {{
            let token = self.expect_peek_token($start_span)?;
            end_span = token.span();
            match token.inner() {
                &Token::Ident(name) => {
                    self.tokens.next();
                    let fields: Option<StructFields> =
                        match self.tokens.peek().map(Spanned::as_pair) {
                            Some((Token::BraceOpen, span)) => {
                                self.tokens.next();
                                let (fields, span) = self.parse_struct_fields(span)?;
                                end_span = span;
                                Some(fields)
                            }
                            _ => None,
                        };
                    let span = $start_span.join(end_span);
                    (Some(name), fields, span)
                }
                Token::BraceOpen => {
                    self.tokens.next();
                    let (fields, span) = self.parse_struct_fields($start_span)?;
                    end_span = span;
                    let span = $start_span.join(end_span);
                    (None, Some(fields), span)
                }
                _ => {
                    self.err_reporter
                        .report(&Error::AnonStructUnion.to_spanned($start_span));
                    (None, None, $start_span)
                }
            }
        }}
        while let Some(token) = self.tokens.peek() {
            if decl_speci.add_token(&self.err_reporter, token).is_ok() {
                end_span = token.span();
                self.tokens.next();
            } else {
                match token.as_pair() {
                    (&Token::Ident(s), span) => {
                        if self.is_typename(s) {
                            {
                                self.tokens.next();
                                decl_speci.add_typename(s.to_spanned(span));
                            }
                        } else {
                            break;
                        }
                    }
                    (Token::Struct, span) => {
                        self.tokens.next();
                        let (name, fields, span) = parse_struct_union!(span);
                        decl_speci.add_struct((name, fields).to_spanned(span));
                    }
                    (Token::Union, span) => {
                        self.tokens.next();
                        let (name, fields, span) = parse_struct_union!(span);
                        decl_speci.add_union((name, fields).to_spanned(span));
                    }
                    (Token::Enum, span) => {
                        self.tokens.next();
                        let (name, fields, span) = {
                            let token = self.expect_peek_token(span)?;
                            end_span = token.span();
                            match token.inner() {
                                &Token::Ident(name) => {
                                    self.tokens.next();
                                    let fields: Option<EnumFields> =
                                        match self.tokens.peek().map(Spanned::as_pair) {
                                            Some((Token::BraceOpen, span)) => {
                                                self.tokens.next();
                                                let (fields, span) =
                                                    self.parse_enum_fields(span)?;
                                                end_span = span;
                                                Some(fields)
                                            }
                                            _ => None,
                                        };
                                    let span = span.join(end_span);
                                    (Some(name), fields, span)
                                }
                                Token::BraceOpen => {
                                    self.tokens.next();
                                    let (fields, span) = self.parse_enum_fields(span)?;
                                    end_span = span;
                                    let span = span.join(end_span);
                                    (None, Some(fields), span)
                                }
                                _ => {
                                    self.err_reporter.report(&Error::AnonEnum.to_spanned(span));
                                    (None, None, span)
                                }
                            }
                        };
                        decl_speci.add_enum((name, fields).to_spanned(span));
                    }
                    _ => break,
                }
            }
        }
        Some(decl_speci.to_spanned(prev_span.join(end_span)))
    }

    /// Deduce a type from decl specifiers.
    fn deduce_ty_speci(&mut self, decl_speci: &Spanned<DeclSpecifiers>) -> Spanned<Ty> {
        macro signness() {
            decl_speci
                .signness
                .map(|s| s.into_inner())
                .unwrap_or(Signness::Signed)
        }
        macro ensure_no {
            (struct_, $err:tt $(,)?) => {
                if let Some(struct_) = &decl_speci.struct_ {
                    self.err_reporter
                        .report(&Error::$err.to_spanned(struct_.span()));
                }
            },
            (union_, $err:tt $(,)?) => {
                if let Some(union_) = &decl_speci.union_ {
                    self.err_reporter
                        .report(&Error::$err.to_spanned(union_.span()));
                }
            },
            (signness $(,)?) => {
                if let Some(signness) = decl_speci.signness {
                    self.err_reporter
                        .report(&Error::InvalidSignnessFlag.to_spanned(signness.span()));
                }
            },
            (typename, $err:tt $(,)?) => {
                if let Some(typename) = decl_speci.typename {
                    self.err_reporter
                        .report(&Error::$err.to_spanned(typename.span()));
                }
            },
            ($speci:tt, $err:tt $(,)?) => {
                if let Some(span) = decl_speci.$speci {
                    self.err_reporter.report(&Error::$err.to_spanned(span));
                }
            },
        }
        // TODO: more accurate span.
        if decl_speci.void.is_some() {
            ensure_no!(char, ConflictingTypeSpecifier);
            ensure_no!(short, ConflictingTypeSpecifier);
            ensure_no!(int, ConflictingTypeSpecifier);
            ensure_no!(short, ConflictingTypeSpecifier);
            ensure_no!(long0, ConflictingTypeSpecifier);
            ensure_no!(float, ConflictingTypeSpecifier);
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            ensure_no!(struct_, ConflictingTypeSpecifier);
            ensure_no!(union_, ConflictingTypeSpecifier);
            ensure_no!(typename, ConflictingTypeSpecifier);
            TyKind::Void
        } else if decl_speci.char.is_some() {
            ensure_no!(short, ConflictingTypeSpecifier);
            ensure_no!(int, ConflictingTypeSpecifier);
            ensure_no!(short, ConflictingTypeSpecifier);
            ensure_no!(long0, ConflictingTypeSpecifier);
            ensure_no!(float, ConflictingTypeSpecifier);
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            ensure_no!(struct_, ConflictingTypeSpecifier);
            ensure_no!(union_, ConflictingTypeSpecifier);
            ensure_no!(typename, ConflictingTypeSpecifier);
            TyKind::Int(signness!(), IntSize::_8)
        } else if decl_speci.short.is_some() {
            ensure_no!(int, ConflictingTypeSpecifier);
            ensure_no!(long0, ConflictingTypeSpecifier);
            ensure_no!(float, ConflictingTypeSpecifier);
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            ensure_no!(struct_, ConflictingTypeSpecifier);
            ensure_no!(union_, ConflictingTypeSpecifier);
            ensure_no!(typename, ConflictingTypeSpecifier);
            TyKind::Int(signness!(), IntSize::_16)
        } else if decl_speci.int.is_some() {
            ensure_no!(long0, ConflictingTypeSpecifier);
            ensure_no!(float, ConflictingTypeSpecifier);
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            ensure_no!(struct_, ConflictingTypeSpecifier);
            ensure_no!(union_, ConflictingTypeSpecifier);
            ensure_no!(typename, ConflictingTypeSpecifier);
            TyKind::Int(signness!(), IntSize::_32)
        } else if decl_speci.long0.is_some() {
            ensure_no!(float, ConflictingTypeSpecifier);
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            ensure_no!(struct_, ConflictingTypeSpecifier);
            ensure_no!(union_, ConflictingTypeSpecifier);
            ensure_no!(typename, ConflictingTypeSpecifier);
            TyKind::Int(signness!(), IntSize::_64)
        } else if decl_speci.float.is_some() {
            ensure_no!(double, ConflictingTypeSpecifier);
            ensure_no!(bool, ConflictingTypeSpecifier);
            ensure_no!(signness);
            ensure_no!(struct_, ConflictingTypeSpecifier);
            ensure_no!(union_, ConflictingTypeSpecifier);
            ensure_no!(typename, ConflictingTypeSpecifier);
            TyKind::Float(FloatSize::_32)
        } else if decl_speci.double.is_some() {
            ensure_no!(signness);
            ensure_no!(bool, ConflictingTypeSpecifier);
            ensure_no!(struct_, ConflictingTypeSpecifier);
            ensure_no!(union_, ConflictingTypeSpecifier);
            ensure_no!(typename, ConflictingTypeSpecifier);
            TyKind::Float(FloatSize::_64)
        } else if decl_speci.bool.is_some() {
            ensure_no!(signness);
            ensure_no!(struct_, ConflictingTypeSpecifier);
            ensure_no!(union_, ConflictingTypeSpecifier);
            ensure_no!(typename, ConflictingTypeSpecifier);
            TyKind::Bool
        } else if let Some(typename) = decl_speci.typename {
            ensure_no!(signness);
            ensure_no!(struct_, ConflictingTypeSpecifier);
            ensure_no!(union_, ConflictingTypeSpecifier);
            let ty = self.ty_of_typename(*typename).unwrap();
            return Rc::new(Ty_ {
                is_const: ty.is_const | decl_speci.const_.is_some(),
                is_volatile: ty.is_volatile | decl_speci.volatile.is_some(),
                kind: ty.kind.clone(),
                typename: Some(*typename),
            })
            .to_spanned(decl_speci.span());
        } else if let Some(struct_) = &decl_speci.struct_ {
            ensure_no!(signness);
            ensure_no!(union_, ConflictingTypeSpecifier);
            TyKind::Struct(struct_.0, struct_.1.clone())
        } else if let Some(union_) = &decl_speci.union_ {
            ensure_no!(signness);
            TyKind::Union(union_.0, union_.1.clone())
        } else if let Some(enum_) = &decl_speci.enum_ {
            ensure_no!(signness);
            TyKind::Enum(enum_.0, enum_.1.clone())
        } else if let Some(signness) = decl_speci.signness {
            ensure_no!(struct_, ConflictingTypeSpecifier);
            ensure_no!(union_, ConflictingTypeSpecifier);
            ensure_no!(typename, ConflictingTypeSpecifier);
            TyKind::Int(signness.into_inner(), IntSize::_32)
        } else {
            self.err_reporter
                .report(&Error::ExpectTyExpr.to_spanned(decl_speci.span()));
            TyKind::Error
        }
        .to_ty(
            decl_speci.const_.is_some(),
            decl_speci.volatile.is_some(),
            None,
        )
        .to_spanned(decl_speci.span())
    }

    /// Parse (possible) array declarators.
    fn parse_arr_decl(&mut self, mut ty: Spanned<Ty>) -> Option<Spanned<Ty>> {
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
                            ty = TyKind::Ptr(Restrictness::NoRestrict, ty.into_inner())
                                .to_ty(false, false, None)
                                .to_spanned(span);
                        }
                        &Token::NumLiteral(NumValue::I(i)) => {
                            self.tokens.next();
                            let prev_ty_span = ty.span();
                            let closing_bracket_span =
                                expect_token!(self, Token::BracketClose, span);
                            if let Ok(i) = u64::try_from(i) {
                                ty = TyKind::FixedArr(ty.into_inner(), i)
                                    .to_ty(false, false, None)
                                    .to_spanned(prev_ty_span.join(closing_bracket_span));
                            } else {
                                self.err_reporter
                                    .report(&Error::IllegalArrLen.to_spanned(span));
                                ty = Ty_::error()
                                    .to_spanned(prev_ty_span.join(closing_bracket_span));
                            }
                        }
                        Token::NumLiteral(NumValue::F(_)) => {
                            let prev_ty_span = ty.span();
                            let closing_bracket_span =
                                expect_token!(self, Token::BracketClose, span);
                            self.err_reporter
                                .report(&Error::IllegalArrLen.to_spanned(span));
                            ty = Ty_::error().to_spanned(prev_ty_span.join(closing_bracket_span));
                        }
                        _ => {
                            self.err_reporter
                                .report(&Error::Todo("variable array length").to_spanned(span));
                            let span = self.parse_expr(opening_bracket_span, 16)?.span();
                            let prev_ty_span = ty.span();
                            let closing_bracket_span =
                                expect_token!(self, Token::BracketClose, span);
                            ty = Ty_::error().to_spanned(prev_ty_span.join(closing_bracket_span));
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
                        ty.into_inner(),
                    )
                    .to_ty(const_.is_some(), volatile.is_some(), None)
                    .to_spanned(span);
                }
                _ => break,
            }
        }
        ty
    }

    fn parse_fn_decl_args(
        &mut self,
        prev_span: Span,
    ) -> Option<Vec<(Ty, Option<Spanned<IdentStr>>)>> {
        let mut args = Vec::<(Ty, Option<Spanned<IdentStr>>)>::new();
        let token = self.expect_peek_token(prev_span)?;
        if token.inner() == &Token::ParenClose {
            self.tokens.next();
        } else {
            loop {
                let mut prev_span = prev_span;
                let decl_speci = DeclSpecifiers::default();
                let decl_speci = self.parse_decl_speci(prev_span, decl_speci)?;
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
                args.push((ty.into_inner(), Some(ident.to_spanned(ident_span))));
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
        let root_ty = self.deduce_ty_speci(decl_speci);
        let ty = self.parse_ptr_decl(root_ty.clone()); // `.clone()` for possible list decl. Root type is simple so cloning is really fast.
        let ty_span = ty.span();
        let (&Token::Ident(ident), ident_span) = self.expect_peek_token(ty_span)?.as_pair() else {
            return Some(Expr::EmptyDecl(ty.into_inner()).to_spanned(ty_span));
        };

        self.tokens.next();

        let ty = self.parse_arr_decl(ty)?;

        match self.tokens.peek() {
            Some(t) if t.inner() == &Token::Comma => todo!("list decls"),
            Some(t) if t.inner() == &Token::Eq => {
                self.tokens.next();
                let var_specs = decl_speci.try_into_var_speci(&self.err_reporter);
                let rhs = self.parse_expr(ident_span, 16)?;
                let rhs_span = rhs.span();
                Some(
                    Expr::Decl(DeclItem::Var(
                        var_specs,
                        ty,
                        ident.to_spanned(ident_span),
                        Some(rhs.into_boxed()),
                    ))
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
                            Expr::Decl(DeclItem::Func(
                                func_speci,
                                sig,
                                ident.to_spanned(ident_span),
                                Some(body),
                            ))
                            .to_spanned(ty_span.join(span)),
                        )
                    }
                    Some((Token::Comma, _)) => todo!("list decls"),
                    Some(..) | None => Some(
                        Expr::Decl(DeclItem::Func(
                            func_speci,
                            sig,
                            ident.to_spanned(ident_span),
                            None,
                        ))
                        .to_spanned(ty_span.join(prev_span)),
                    ),
                }
            }
            _ => {
                let var_specs = decl_speci.try_into_var_speci(&self.err_reporter);
                Some(
                    Expr::Decl(DeclItem::Var(
                        var_specs,
                        ty,
                        ident.to_spanned(ident_span),
                        None,
                    ))
                    .to_spanned(ty_span.join(ident_span)),
                )
            }
        }
    }

    /// Parse an entire decl statement.
    /// Unlike most `parse_` functions, this function does not start with the first token already consumed.
    fn parse_decl(&mut self, prev_span: Span) -> Option<Spanned<Expr>> {
        let decl_speci = DeclSpecifiers::default();
        let decl_speci = self.parse_decl_speci(prev_span, decl_speci)?;
        self.parse_decl_content(prev_span, &decl_speci)
    }

    /// Start with `{` token already consumed.
    fn parse_block(&mut self, mut prev_span: Span) -> Option<Spanned<ExprBlock>> {
        self.enters_block();
        let start_span = prev_span;
        let mut body = Vec::<Spanned<Expr>>::new();
        self.consume_semicolons(prev_span, true);
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
                    prev_span = self.consume_semicolons(expr.span(), expr.allow_omit_semicolon());
                    body.push(expr);
                }
            }
        }
        self.leaves_block();
        Some(body.to_spanned(start_span.join(prev_span)))
    }

    /// Also works on union fields.
    /// Returns the fields and the end span.
    fn parse_struct_fields(&mut self, prev_span: Span) -> Option<(StructFields, Span)> {
        let mut end_span = prev_span;
        let mut fields: StructFields = Vec::new();
        end_span = self.consume_semicolons(end_span, true);
        loop {
            let (token, span) = self.expect_peek_token(end_span)?.as_pair();
            end_span = span;
            match token {
                Token::BraceClose => {
                    self.tokens.next();
                    break;
                }
                _ => {
                    // Parse the block mostly as if it's a normal code block.
                    let expr = self.parse_expr(end_span, 16)?;
                    end_span = expr.span();
                    let mut omits_semicolon = expr.allow_omit_semicolon();
                    match expr.into_inner() {
                        Expr::Decl(DeclItem::Var(speci, ty, name, rhs)) => {
                            if speci != VarSpecifier::Unspecified {
                                self.err_reporter
                                    .report(&Error::StorageClassInStructUnion.to_spanned(end_span));
                            }
                            rhs.inspect(|rhs| {
                                self.err_reporter
                                    .report(&Error::RhsInStructUnion.to_spanned(rhs.span()))
                            });
                            fields.push((ty.into_inner(), *name));
                            omits_semicolon = true;
                        }
                        _ => {
                            self.err_reporter
                                .report(&Error::NonDeclInStructUnion.to_spanned(end_span));
                        }
                    }
                    end_span = self.consume_semicolons(end_span, omits_semicolon);
                }
            }
        }
        Some((fields, end_span))
    }

    /// Returns the fields and the end span.
    fn parse_enum_fields(&mut self, prev_span: Span) -> Option<(EnumFields, Span)> {
        let mut end_span = prev_span;
        let mut fields: EnumFields = Vec::new();
        loop {
            let (token, span) = self.expect_peek_token(end_span)?.as_pair();
            end_span = span;
            match token {
                Token::BraceClose => {
                    self.tokens.next();
                    break;
                }
                &Token::Ident(ident) => {
                    self.tokens.next();
                    let value = match self.tokens.peek().map(Spanned::as_pair) {
                        Some((Token::Eq, span)) => {
                            self.tokens.next();
                            Some(self.parse_expr(span, 15)?.into_boxed())
                        }
                        _ => None,
                    };
                    fields.push((ident, value));
                    match self.expect_peek_token(end_span)?.as_pair() {
                        (Token::Comma, span) => {
                            self.tokens.next();
                            end_span = span;
                        }
                        (Token::BraceClose, span) => {
                            self.tokens.next();
                            end_span = span;
                            break;
                        }
                        (_, span) => self.err_reporter.report(
                            &Error::ExpectTokens(&[Token::Comma, Token::BraceClose])
                                .to_spanned(span),
                        ),
                    }
                }
                _ => {
                    self.err_reporter.report(
                        &Error::ExpectTokens(&[Token::Ident(intern_string("")), Token::BraceClose])
                            .to_spanned(span),
                    );
                    break;
                }
            }
        }
        Some((fields, end_span))
    }

    /// Start with opening paren already consumed.
    fn parse_paren(&mut self, prev_span: Span) -> Option<Spanned<Expr>> {
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
                let decl_speci = self.parse_decl_speci(prev_span, decl_speci)?;
                let ty = self.deduce_ty_speci(&decl_speci);
                let closing_paren_span = expect_token!(self, Token::ParenClose, decl_speci.span());
                let expr = self.parse_expr(closing_paren_span, 2)?;
                let span = prev_span.join(expr.span());
                Some(Expr::Typecast(ty.into_inner(), expr.into_boxed()).to_spanned(span))
            }
            &Token::Ident(s) => {
                if self.is_typename(s) {
                    self.tokens.next();
                    let mut decl_speci = DeclSpecifiers::default();
                    decl_speci.add_typename(s.to_spanned(span));
                    let decl_speci = self.parse_decl_speci(prev_span, decl_speci)?;
                    let ty = self.deduce_ty_speci(&decl_speci);
                    let closing_paren_span =
                        expect_token!(self, Token::ParenClose, decl_speci.span());
                    let expr = self.parse_expr(closing_paren_span, 2)?;
                    let span = prev_span.join(expr.span());
                    Some(Expr::Typecast(ty.into_inner(), expr.into_boxed()).to_spanned(span))
                } else {
                    let expr = self.parse_expr(span, 16)?;
                    expect_token!(self, Token::ParenClose, expr.span());
                    // FIXME: ensure it's not a statement.
                    Some(expr)
                }
            }
            _ => {
                let expr = self.parse_expr(span, 16)?;
                expect_token!(self, Token::ParenClose, expr.span());
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
        Some(Expr::Call(callee.into_boxed(), args).to_spanned(span))
    }

    /// Start with the first `.` or `->` not consumed.
    fn parse_field_path(&mut self, root: Spanned<Expr>) -> Option<Spanned<Expr>> {
        let mut items = Vec::<FieldPathItem>::new();
        let mut end_span = root.span();
        while let Some(token) = self.tokens.peek() {
            match token.as_pair() {
                (Token::Dot, span) => {
                    self.tokens.next();
                    let ident = self.expect_ident(span)?;
                    end_span = ident.span();
                    items.push(FieldPathItem::Dir(span, ident));
                }
                (Token::Arrow, span) => {
                    self.tokens.next();
                    let ident = self.expect_ident(span)?;
                    end_span = ident.span();
                    items.push(FieldPathItem::Ind(span, ident));
                }
                _ => break,
            }
        }
        let span = root.span().join(end_span);
        Some(Expr::FieldPath(root.into_boxed(), items).to_spanned(span))
    }

    fn parse_expr_or_block(
        &mut self,
        prev_span: Span,
        take_semicolon: bool,
    ) -> Option<ExprOrBlock> {
        match self.expect_peek_token(prev_span)?.as_pair() {
            (Token::BraceOpen, span) => {
                self.tokens.next();
                let block = self.parse_block(span)?;
                Some(block.into())
            }
            (_, span) => {
                let expr = self.parse_expr(span, 16)?;
                if take_semicolon {
                    self.consume_semicolons(span, expr.allow_omit_semicolon());
                }
                Some(expr.into())
            }
        }
    }

    fn parse_expr(&mut self, prev_span: Span, prec: u16) -> Option<Spanned<Expr>> {
        let token = self.expect_peek_token(prev_span)?;
        let span = token.span();
        macro parse_prefix_op($prec:expr, $opkind:expr $(,)?) {{
            self.tokens.next();
            let operand = self.parse_expr(span, $prec)?;
            let operand_span = operand.span();
            Some(Expr::PrefixOp($opkind, operand.into_boxed()).to_spanned(span.join(operand_span)))
        }}
        let expr = match token.inner() {
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
                let outer_operand = Expr::PrefixOp(PrefixOpKind::Ref, inner_operand.into_boxed())
                    .to_spanned((span.file, span.start + 1, inner_span.end));
                Some(
                    Expr::PrefixOp(PrefixOpKind::Ref, outer_operand.into_boxed()).to_spanned((
                        span.file,
                        span.start,
                        inner_span.end,
                    )),
                )
            }
            &Token::Ident(s) => {
                self.tokens.next();
                if self.is_typename(s) {
                    let mut decl_speci = DeclSpecifiers::default();
                    decl_speci.add_typename(s.to_spanned(span));
                    let decl_speci = self.parse_decl_speci(span, decl_speci)?;
                    let expr = self.parse_decl_content(decl_speci.span(), &decl_speci);
                    expr.as_ref().inspect(|expr| match expr.inner() {
                        &Expr::Decl(DeclItem::Var(VarSpecifier::Typedef, ref ty, name, _)) => {
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
                    match_into_unchecked!(
                        self.tokens.next().unwrap_unchecked().into_inner(),
                        Token::StrLiteral(s) => s)
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
                    &Expr::Decl(DeclItem::Var(VarSpecifier::Typedef, ref ty, name, _)) => {
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
            Token::While => {
                self.tokens.next();
                let start_span = span;
                let span = expect_token!(self, Token::ParenOpen, span);
                let cond = self.parse_expr(span, 16)?;
                let span = expect_token!(self, Token::ParenClose, cond.span());
                let body = self.parse_expr_or_block(span, false)?;
                let span = start_span.join(body.span());
                Some(Expr::While(cond.into_boxed(), body).to_spanned(span))
            }
            Token::Do => {
                self.tokens.next();
                let start_span = span;
                let body = self.parse_expr_or_block(span, true)?;
                let span = body.span();
                let span = expect_token!(self, Token::While, span);
                let span = expect_token!(self, Token::ParenOpen, span);
                let cond = self.parse_expr(span, 16)?;
                let span = expect_token!(self, Token::ParenClose, cond.span());
                Some(Expr::DoWhile(body, cond.into_boxed()).to_spanned(start_span.join(span)))
            }
            Token::Switch => {
                self.tokens.next();
                let start_span = span;
                let span = expect_token!(self, Token::ParenOpen, span);
                let cond = self.parse_expr(span, 16)?;
                let span = expect_token!(self, Token::ParenClose, cond.span());
                let body = self.parse_expr_or_block(span, false)?;
                let span = start_span.join(body.span());
                Some(Expr::Switch(cond.into_boxed(), body).to_spanned(span))
            }
            Token::For => {
                self.tokens.next();
                let start_span = span;
                let span = expect_token!(self, Token::ParenOpen, span);
                macro parse_fragment($prev_span:expr, $end_token:path) {{
                    let prev_span = $prev_span;
                    match self.expect_peek_token(prev_span)?.inner() {
                        $end_token => {
                            self.tokens.next();
                            (None, prev_span)
                        }
                        _ => {
                            let expr = self.parse_expr(prev_span, 16)?;
                            let span = expect_token!(self, $end_token, expr.span());
                            (Some(expr), span)
                        }
                    }
                }}
                let (expr0, span) = parse_fragment!(span, Token::Semicolon);
                let (expr1, span) = parse_fragment!(span, Token::Semicolon);
                let (expr2, span) = parse_fragment!(span, Token::ParenClose);
                let body = self.parse_expr_or_block(span, false)?;
                let span = start_span.join(body.span());
                Some(
                    Expr::For(
                        expr0.map(Spanned::into_boxed),
                        expr1.map(Spanned::into_boxed),
                        expr2.map(Spanned::into_boxed),
                        body,
                    )
                    .to_spanned(span),
                )
            }
            Token::If => {
                self.tokens.next();
                let start_span = span;
                macro parse_elif_branch($prev_span:expr) {{
                    let span = expect_token!(self, Token::ParenOpen, $prev_span);
                    let cond = self.parse_expr(span, 16)?;
                    let span = expect_token!(self, Token::ParenClose, cond.span());
                    let body = self.parse_expr_or_block(span, true)?;
                    (cond, body)
                }}
                let (cond, body) = parse_elif_branch!(span);
                let mut end_span = body.span();
                let mut elifs = Vec::from_iter([(cond, body)]);
                let mut else_ = Option::<ExprOrBlock>::None;
                while let Some(token) = self.tokens.peek() {
                    let Some(span) = match_into!(token.as_pair(), (Token::Else, span) => span)
                    else {
                        break;
                    };
                    self.tokens.next();
                    match self.expect_peek_token(span)?.as_pair() {
                        (Token::If, span) => {
                            self.tokens.next();
                            let (cond, body) = parse_elif_branch!(span);
                            end_span = body.span();
                            elifs.push((cond, body))
                        }
                        _ => {
                            let body = self.parse_expr_or_block(span, false)?;
                            end_span = body.span();
                            else_ = Some(body);
                            break;
                        }
                    }
                }
                Some(Expr::If(elifs, else_).to_spanned(start_span.join(end_span)))
            }
            Token::Goto => {
                self.tokens.next();
                if let Some((ident, ident_span)) = self.expect_ident(span).map(Spanned::into_pair) {
                    Some(Expr::Goto(ident).to_spanned(span.join(ident_span)))
                } else {
                    return Some(Expr::Error.to_spanned(span));
                }
            }
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
                            Expr::Return(Some(operand.into_boxed()))
                                .to_spanned(span.join(operand_span)),
                        )
                    }
                }
            }
            Token::Sizeof => todo!("sizeof"),
            Token::ParenOpen => {
                self.tokens.next();
                self.parse_paren(span)
            }
            Token::Case => {
                self.tokens.next();
                let expr = self.parse_expr(span, 16)?;
                let end_span = expect_token!(self, Token::Colon, expr.span());
                Some(Expr::Case(expr.into_boxed()).to_spanned(span.join(end_span)))
            }
            Token::Default => {
                self.tokens.next();
                let end_span = expect_token!(self, Token::Colon, span);
                Some(Expr::Default.to_spanned(span.join(end_span)))
            }
            Token::BraceOpen => {
                self.tokens.next();
                let mut items = Vec::<ListItem>::new();
                let start_span = span;
                let mut end_span = span;
                loop {
                    let (token, span) = self.expect_peek_token(end_span)?.as_pair();
                    end_span = span;
                    match token {
                        Token::BraceClose => {
                            self.tokens.next();
                            break;
                        }
                        Token::Dot => {
                            self.tokens.next();
                            let ident = self.expect_ident(end_span)?;
                            end_span = expect_token!(self, Token::Eq, ident.span());
                            let expr = self.parse_expr(end_span, 15)?;
                            items.push(ListItem::Field(ident, expr.into_boxed()))
                        }
                        Token::BracketOpen => {
                            self.tokens.next();
                            let operand = self.parse_expr(end_span, 16)?;
                            end_span = expect_token!(self, Token::BracketClose, operand.span());
                            end_span = expect_token!(self, Token::Eq, end_span);
                            let expr = self.parse_expr(end_span, 15)?;
                            items.push(ListItem::Index(operand.into_boxed(), expr.into_boxed()))
                        }
                        _ => {
                            let expr = self.parse_expr(end_span, 15)?;
                            items.push(ListItem::Expr(expr.into_boxed()));
                        }
                    }
                    match self.expect_peek_token(end_span)?.as_pair() {
                        (Token::Comma, span) => {
                            end_span = span;
                            self.tokens.next();
                        }
                        (Token::BraceClose, span) => {
                            end_span = span;
                            self.tokens.next();
                            break;
                        }
                        (_, span) => {
                            self.err_reporter.report(
                                &Error::ExpectTokens(&[Token::Comma, Token::BraceClose])
                                    .to_spanned(span),
                            );
                            return None;
                        }
                    }
                }
                Some(Expr::List(items).to_spanned(start_span.join(end_span)))
            }
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
            macro infix_op($exprkind:tt, $kind:expr, $prec:expr $(,)?) {{
                if prec <= $prec {
                    break;
                }
                self.tokens.next();
                let rhs = self
                    .parse_expr(span, $prec)
                    .unwrap_or_else(|| Expr::Error.to_spanned(span));
                let span = expr.span().join(rhs.span());
                expr = Expr::$exprkind(expr.into_boxed(), $kind, rhs.into_boxed()).to_spanned(span);
            }}
            macro postfix_op($kind:expr $(,)?) {{
                // The only two cases are postfix++ and postfix--, both having precedence of 1,
                // so no need for precedence checking.
                self.tokens.next();
                let span = expr.span().join(span);
                expr = Expr::PostfixOp(expr.into_boxed(), $kind).to_spanned(span);
            }}
            match token {
                Token::Ques => {
                    self.tokens.next();
                    let start_span = expr.span();
                    let middle = self.parse_expr(span, 16)?;
                    let span = expect_token!(self, Token::Colon, middle.span());
                    let right = self.parse_expr(span, 13)?;
                    let span = start_span.join(right.span());
                    expr = Expr::Tenary(expr.into_boxed(), middle.into_boxed(), right.into_boxed())
                        .to_spanned(span);
                }
                // Precedence 1. No need for precedence checking here.
                Token::ParenOpen => {
                    self.tokens.next();
                    expr = self.parse_call(span, expr)?;
                }
                Token::BracketOpen => {
                    self.tokens.next();
                    let operand = self.parse_expr(span, 16)?;
                    let end_span = expect_token!(self, Token::BracketClose, operand.span());
                    let span = expr.span().join(end_span);
                    expr =
                        Expr::Subscript(expr.into_boxed(), operand.into_boxed()).to_spanned(span);
                }
                Token::Dot => {
                    expr = self.parse_field_path(expr)?;
                }
                Token::Arrow => {
                    expr = self.parse_field_path(expr)?;
                }
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
                Token::Eq => infix_op!(InfixOp, InfixOpKind::Assign, 7),
                Token::EqEq => infix_op!(InfixOp, InfixOpKind::Eq, 7),
                Token::ExcEq => infix_op!(InfixOp, InfixOpKind::Ne, 7),
                Token::And => infix_op!(InfixOp, InfixOpKind::BitAnd, 8),
                Token::Xor => infix_op!(InfixOp, InfixOpKind::BitXor, 9),
                Token::Or => infix_op!(InfixOp, InfixOpKind::BitOr, 10),
                Token::AndAnd => infix_op!(InfixOp, InfixOpKind::And, 11),
                Token::OrOr => infix_op!(InfixOp, InfixOpKind::Or, 12),
                // Assignments technically have right-to-left associativity, but most compilers don't care,
                // as only expressions with very high (aka. small) precedence can legally be the LHS of assignments.
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

    fn consume_semicolons(&mut self, mut prev_span: Span, allows_omit: bool) -> Span {
        let token = self.tokens.peek();
        match token.map(Spanned::as_pair) {
            Some((Token::Semicolon, span)) => {
                self.tokens.next();
                prev_span = span;
            }
            _ => {
                dbg!();
                if !allows_omit {
                    self.err_reporter
                        .report(&Error::MissingSemicolon.to_spanned(prev_span.tail()));
                }
            }
        }
        while let Some(token) = self.tokens.peek() {
            match token.as_pair() {
                (Token::Semicolon, span) => {
                    self.tokens.next();
                    prev_span = span;
                }
                _ => break,
            }
        }
        prev_span
    }
}

impl Iterator for Parser {
    type Item = Spanned<Expr>;

    fn next(&mut self) -> Option<Self::Item> {
        let span = self.tokens.peek()?.span();
        let expr = self.parse_expr(span, 16)?;
        self.consume_semicolons(expr.span(), expr.allow_omit_semicolon());
        Some(expr)
    }
}
