use std::char;
use std::collections::HashMap;
use std::fmt::{self, Debug};
use std::iter::Peekable;
use std::rc::Rc;

use crate::error::{Error, ErrorReporter, Span, Spanned, ToSpanned};
use crate::file_reader::FileReader;
use crate::intern_string;
use crate::source_string::{SourceCharIndices, SourceIdx, SourceString};
use crate::utils::{derive_display_from_debug, match_into, IdentStr};

#[derive(Clone, PartialEq)]
pub enum Token {
    Ident(IdentStr),
    NumLiteral(NumValue),
    StrLiteral(Rc<[u8]>),
    CharLiteral(u8),

    Auto,
    Break,
    Case,
    Char,
    Const,
    Continue,
    Default,
    Do,
    Double,
    Else,
    Enum,
    Extern,
    Float,
    For,
    Goto,
    If,
    Inline,
    Int,
    Long,
    Register,
    Restrict,
    Return,
    Short,
    Signed,
    Sizeof,
    Static,
    Struct,
    Switch,
    Typedef,
    Union,
    Unsigned,
    Void,
    Volatile,
    While,
    /// `_Bool` keyword.
    Bool,
    /// `_Complex` keyword.
    Complex,
    /// `_Imaginary` keyword.
    Imaginary,

    Add,
    AddAdd,
    AddEq,
    Sub,
    SubSub,
    SubEq,
    Arrow,
    Mul,
    MulEq,
    Div,
    DivEq,
    Rem,
    RemEq,
    Lt,
    LtLt,
    LtLtEq,
    LtEq,
    Gt,
    GtGt,
    GtGtEq,
    GtEq,
    Tilde,
    Exc,
    ExcEq,
    And,
    AndAnd,
    AndEq,
    Eq,
    EqEq,
    Or,
    OrOr,
    OrEq,
    Xor,
    XorEq,
    Dot,
    Comma,
    Colon,
    Semicolon,
    Ques,

    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,

    Backslash,
}
impl Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(s) => write!(f, "id({s})"),
            Self::NumLiteral(n) => write!(f, "num({n:?})"),
            Self::StrLiteral(bytes) => match core::str::from_utf8(&bytes) {
                // TODO: print string in valid region, bytes in invalid region.
                Ok(s) => write!(f, "str({s:?})"),
                Err(..) => write!(f, "str({bytes:?})"),
            },
            Self::CharLiteral(c) => write!(f, "char({c:?})"),
            Self::Auto => write!(f, "keyword(auto)"),
            Self::Break => write!(f, "keyword(break)"),
            Self::Case => write!(f, "keyword(case)"),
            Self::Char => write!(f, "keyword(char)"),
            Self::Const => write!(f, "keyword(const)"),
            Self::Continue => write!(f, "keyword(continue)"),
            Self::Default => write!(f, "keyword(default)"),
            Self::Do => write!(f, "keyword(do)"),
            Self::Double => write!(f, "keyword(double)"),
            Self::Else => write!(f, "keyword(else)"),
            Self::Enum => write!(f, "keyword(enum)"),
            Self::Extern => write!(f, "keyword(extern)"),
            Self::Float => write!(f, "keyword(float)"),
            Self::For => write!(f, "keyword(for)"),
            Self::Goto => write!(f, "keyword(goto)"),
            Self::If => write!(f, "keyword(if)"),
            Self::Inline => write!(f, "keyword(inline)"),
            Self::Int => write!(f, "keyword(int)"),
            Self::Long => write!(f, "keyword(long)"),
            Self::Register => write!(f, "keyword(register)"),
            Self::Restrict => write!(f, "keyword(restrict)"),
            Self::Return => write!(f, "keyword(return)"),
            Self::Short => write!(f, "keyword(short)"),
            Self::Signed => write!(f, "keyword(signed)"),
            Self::Sizeof => write!(f, "keyword(sizeof)"),
            Self::Static => write!(f, "keyword(static)"),
            Self::Struct => write!(f, "keyword(struct)"),
            Self::Switch => write!(f, "keyword(switch)"),
            Self::Typedef => write!(f, "keyword(typedef)"),
            Self::Union => write!(f, "keyword(union)"),
            Self::Unsigned => write!(f, "keyword(unsigned)"),
            Self::Void => write!(f, "keyword(void)"),
            Self::Volatile => write!(f, "keyword(volatile)"),
            Self::While => write!(f, "keyword(while)"),
            Self::Bool => write!(f, "keyword(_Bool)"),
            Self::Complex => write!(f, "keyword(_Complex)"),
            Self::Imaginary => write!(f, "keyword(_Imaginary)"),
            Self::Add => write!(f, "operator(+)"),
            Self::AddAdd => write!(f, "operator(++)"),
            Self::AddEq => write!(f, "operator(+=)"),
            Self::Sub => write!(f, "operator(-)"),
            Self::SubSub => write!(f, "operator(--)"),
            Self::SubEq => write!(f, "operator(-=)"),
            Self::Arrow => write!(f, "operator(->)"),
            Self::Mul => write!(f, "operator(*)"),
            Self::MulEq => write!(f, "operator(*=)"),
            Self::Div => write!(f, "operator(/)"),
            Self::DivEq => write!(f, "operator(/=)"),
            Self::Rem => write!(f, "operator(%)"),
            Self::RemEq => write!(f, "operator(%=)"),
            Self::Lt => write!(f, "operator(<)"),
            Self::LtLt => write!(f, "operator(<<)"),
            Self::LtLtEq => write!(f, "operator(<<=)"),
            Self::LtEq => write!(f, "operator(<=)"),
            Self::Gt => write!(f, "operator(>)"),
            Self::GtGt => write!(f, "operator(>>)"),
            Self::GtGtEq => write!(f, "operator(>>=)"),
            Self::GtEq => write!(f, "operator(>=)"),
            Self::Tilde => write!(f, "operator(~)"),
            Self::Exc => write!(f, "operator(!)"),
            Self::ExcEq => write!(f, "operator(!=)"),
            Self::And => write!(f, "operator(&)"),
            Self::AndAnd => write!(f, "operator(&&)"),
            Self::AndEq => write!(f, "operator(&=)"),
            Self::Eq => write!(f, "operator(=)"),
            Self::EqEq => write!(f, "operator(==)"),
            Self::Or => write!(f, "operator(|)"),
            Self::OrOr => write!(f, "operator(||)"),
            Self::OrEq => write!(f, "operator(|=)"),
            Self::Xor => write!(f, "operator(^)"),
            Self::XorEq => write!(f, "operator(^=)"),
            Self::Dot => write!(f, "operator(.)"),
            Self::Comma => write!(f, "operator(,)"),
            Self::Colon => write!(f, "operator(:)"),
            Self::Semicolon => write!(f, "operator(;)"),
            Self::Ques => write!(f, "operator(?)"),
            Self::ParenOpen => write!(f, "paren(open)"),
            Self::ParenClose => write!(f, "paren(close)"),
            Self::BracketOpen => write!(f, "bracket[open]"),
            Self::BracketClose => write!(f, "bracket[close]"),
            Self::BraceOpen => write!(f, "brace{{open}}"),
            Self::BraceClose => write!(f, "brace{{close}}"),
            Self::Backslash => write!(f, "operator(\\)"),
        }
    }
}
impl Token {
    /// Is it a declaration specifier?
    /// Returns `false` for identifiers.
    /// Returns `false` for `typedef`.
    pub const fn is_decl_speci(&self) -> bool {
        match self {
            Self::Auto
            | Self::Char
            | Self::Const
            | Self::Double
            | Self::Enum
            | Self::Extern
            | Self::Float
            | Self::Inline
            | Self::Int
            | Self::Long
            | Self::Register
            | Self::Restrict
            | Self::Short
            | Self::Signed
            | Self::Static
            | Self::Struct
            | Self::Typedef
            | Self::Union
            | Self::Unsigned
            | Self::Void
            | Self::Volatile
            | Self::Bool
            | Self::Complex
            | Self::Imaginary => true,
            _ => false,
        }
    }
    pub const fn is_ident(&self) -> bool {
        matches!(self, Self::Ident(..))
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum NumValue {
    I(i128),
    F(f64),
}
derive_display_from_debug!(NumValue);
impl Debug for NumValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::I(n) => write!(f, "{}i", n),
            Self::F(n) => write!(f, "{}f", n),
        }
    }
}
impl From<i64> for NumValue {
    fn from(value: i64) -> Self {
        Self::I(value as i128)
    }
}
impl From<u64> for NumValue {
    fn from(value: u64) -> Self {
        Self::I(value as i128)
    }
}
impl From<i128> for NumValue {
    fn from(value: i128) -> Self {
        Self::I(value)
    }
}
impl From<f64> for NumValue {
    fn from(value: f64) -> Self {
        Self::F(value)
    }
}

/// Content of a macro.
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct MacroContent {
    args: Vec<IdentStr>,
    has_va_arg: bool,
    content: Vec<PpToken>,
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum PpToken {
    Token(Token),
    Slot(IdentStr),
    Stringify,
    Concat,
}

#[derive(Debug, Clone)]
pub struct TokenStream {
    path: &'static str,
    source: &'static SourceString,
    chars: Peekable<SourceCharIndices<'static>>,
    pub macros: HashMap<IdentStr, Option<MacroContent>>,
    macro_buffer: Vec<Token>,
    /// If `macro_buffer` isn't empty (meaning the tokenizer is in a macro), this is the location of the call site of the macro.
    current_macro_callsite: Span,
    err_reporter: Rc<ErrorReporter>,
    #[allow(dead_code)]
    file_reader: Rc<FileReader>,
}

macro hex_char_to_digit($c:expr, $t:ty) {
    ($c as $t)
        .wrapping_sub('0' as $t)
        .min(($c as $t).wrapping_sub('a' as $t - 10))
        .min(($c as $t).wrapping_sub('A' as $t - 10))
}

impl TokenStream {
    pub fn new(
        path: &'static str,
        source: &'static SourceString,
        err_reporter: Rc<ErrorReporter>,
        file_reader: Rc<FileReader>,
    ) -> Self {
        let iter = source.char_indices().peekable();
        Self {
            path,
            source,
            chars: iter,
            macros: HashMap::default(),
            macro_buffer: Vec::new(),
            current_macro_callsite: Span::default(),
            err_reporter,
            file_reader,
        }
    }

    fn take_ident_or_keyword(&mut self, start_idx: SourceIdx) -> Option<Spanned<Token>> {
        let mut end_idx = start_idx;
        while let Some(&(i, c)) = self.chars.peek() {
            if !(c.is_alphanumeric() || c == '_') {
                break;
            }
            end_idx = i;
            self.chars.next();
        }
        let str = &self.source[start_idx..end_idx];
        let token = match str {
            "auto" => Token::Auto,
            "break" => Token::Break,
            "case" => Token::Case,
            "char" => Token::Char,
            "const" => Token::Const,
            "continue" => Token::Continue,
            "default" => Token::Default,
            "do" => Token::Do,
            "double" => Token::Double,
            "else" => Token::Else,
            "enum" => Token::Enum,
            "extern" => Token::Extern,
            "float" => Token::Float,
            "for" => Token::For,
            "goto" => Token::Goto,
            "if" => Token::If,
            "inline" => Token::Inline,
            "int" => Token::Int,
            "long" => Token::Long,
            "register" => Token::Register,
            "restrict" => Token::Restrict,
            "return" => Token::Return,
            "short" => Token::Short,
            "signed" => Token::Signed,
            "sizeof" => Token::Sizeof,
            "static" => Token::Static,
            "struct" => Token::Struct,
            "switch" => Token::Switch,
            "typedef" => Token::Typedef,
            "union" => Token::Union,
            "unsigned" => Token::Unsigned,
            "void" => Token::Void,
            "volatile" => Token::Volatile,
            "while" => Token::While,
            "_Bool" => Token::Bool,
            "_Complex" => Token::Complex,
            "_Imaginary" => Token::Imaginary,
            _ => Token::Ident(intern_string(str)),
        };
        Some(token.to_spanned((self.path, start_idx, end_idx)))
    }

    fn take_float(&mut self, start_idx: SourceIdx) -> Option<Spanned<Token>> {
        let mut end_idx = start_idx;
        while let Some(&(i, c)) = self.chars.peek() {
            let digit = (c as u32).wrapping_sub('0' as u32);
            if matches!(c, 'e' | '.') || digit <= 9 {
                self.chars.next();
                end_idx = i;
            } else {
                break;
            }
        }
        let float = self.source[start_idx..end_idx].parse::<f64>().unwrap();
        Some(Token::NumLiteral(NumValue::F(float)).to_spanned((self.path, start_idx, end_idx)))
    }

    fn take_number(&mut self, start_idx: SourceIdx, first_ch: char) -> Option<Spanned<Token>> {
        let mut end_idx = start_idx;
        let mut val = first_ch as i128 - ('0' as i128);
        if val == 0 {
            // could be 0x, 0o, 0d, 0b prefix numbers
            let (suffix_idx, second_char) = if let Some(&x) = self.chars.peek() {
                x
            } else {
                let token = Token::NumLiteral(0i128.into());
                return Some(token.to_spanned((self.path, start_idx, end_idx)));
            };
            match second_char {
                'x' => {
                    self.chars.next();
                    while let Some((i, c)) = self.chars.peek() {
                        let digit = hex_char_to_digit!(*c, i128);
                        if digit > 15 {
                            end_idx = *i;
                            break;
                        }
                        val *= 16;
                        val += digit;
                        self.chars.next();
                    }
                    let token = Token::NumLiteral(val.into());
                    Some(token.to_spanned((self.path, start_idx, end_idx)))
                }
                'd' => {
                    self.chars.next();
                    while let Some((i, c)) = self.chars.peek() {
                        let digit = (*c as u32).wrapping_sub('0' as u32) as i128;
                        if digit > 9 {
                            end_idx = *i;
                            break;
                        }
                        val *= 10;
                        val += digit;
                        self.chars.next();
                    }
                    let token = Token::NumLiteral(val.into());
                    Some(token.to_spanned((self.path, start_idx, end_idx)))
                }
                'o' => {
                    self.chars.next();
                    while let Some((i, c)) = self.chars.peek() {
                        let digit = (*c as u32).wrapping_sub('0' as u32) as i128;
                        if digit > 7 {
                            end_idx = *i;
                            break;
                        }
                        val *= 8;
                        val += digit;
                        self.chars.next();
                    }
                    let token = Token::NumLiteral(val.into());
                    Some(token.to_spanned((self.path, start_idx, end_idx)))
                }
                'b' => {
                    self.chars.next();
                    while let Some((i, c)) = self.chars.peek() {
                        match *c {
                            '0' => {
                                val <<= 1;
                            }
                            '1' => {
                                val <<= 1;
                                val |= 1;
                            }
                            _ => {
                                end_idx = *i;
                                break;
                            }
                        }
                        self.chars.next();
                    }
                    let token = Token::NumLiteral(val.into());
                    Some(token.to_spanned((self.path, start_idx, end_idx)))
                }
                '.' | 'e' => {
                    self.chars.next();
                    self.take_float(start_idx)
                }
                _ => {
                    val += (second_char as u32).wrapping_sub('0' as u32) as i128;
                    if val > 9 {
                        let token = Token::NumLiteral(0i128.into());
                        return Some(token.to_spanned((self.path, start_idx, end_idx)));
                    }
                    self.chars.next();
                    while let Some(&(i, c)) = self.chars.peek() {
                        let digit = (c as u32).wrapping_sub('0' as u32) as i128;
                        if digit > 9 {
                            end_idx = i;
                            self.err_reporter.report(
                                &Error::InvalidNumberFormat
                                    .to_spanned((self.path, start_idx, suffix_idx)),
                            );
                            return Some(
                                Token::NumLiteral(0i128.into())
                                    .to_spanned((self.path, start_idx, end_idx)),
                            );
                        }
                        val *= 10;
                        val += digit;
                        self.chars.next();
                    }
                    let token = Token::NumLiteral(val.into());
                    Some(token.to_spanned((self.path, start_idx, end_idx)))
                }
            }
        } else {
            while let Some(&(i, c)) = self.chars.peek() {
                if matches!(c, '.' | 'e') {
                    self.chars.next();
                    return self.take_float(start_idx);
                }
                let digit = (c as u32).wrapping_sub('0' as u32) as i128;
                if digit > 9 {
                    end_idx = i;
                    break;
                }
                self.chars.next();
                val *= 10;
                val += digit;
            }
            let token = Token::NumLiteral(val.into());
            Some(token.to_spanned((self.path, start_idx, end_idx)))
        }
    }

    fn parse_string_escape(&mut self, i: SourceIdx) -> Result<u8, Spanned<Error<'static>>> {
        let escape_char = self
            .chars
            .next()
            .ok_or(Error::EofInStringLiteral.to_spanned((self.path, i)))?
            .1;
        match escape_char {
            'x' => {
                let (i0, ch) = self
                    .chars
                    .next()
                    .ok_or(Error::EofInStringLiteral.to_spanned((self.path, i)))?;
                let digit0 = hex_char_to_digit!(ch, u8);
                let (i1, ch) = self
                    .chars
                    .next()
                    .ok_or(Error::EofInStringLiteral.to_spanned((self.path, i0)))?;
                let digit1 = hex_char_to_digit!(ch, u8);
                if digit0 > 15 {
                    Err(Error::InvalidStringEscape.to_spanned((self.path, i1)))
                } else if digit1 > 15 {
                    Err(Error::InvalidStringEscape.to_spanned((self.path, i1)))
                } else {
                    Ok(digit0 * 16 + digit1)
                }
            }
            c if (c as u32) >= ('0' as u32) && (c as u32) <= ('9' as u32) => todo!("octal escape"),
            'u' | 'U' => todo!("unicode escape"),
            'a' => Ok(0x07), // hopefully no mistake here lol
            'n' => Ok(0x0A),
            'r' => Ok(0x0D),
            't' => Ok(0x09),
            '\\' => Ok(0x5C),
            '\'' => Ok(0x27),
            '\"' => Ok(0x22),
            _ => Err(Error::InvalidStringEscape.to_spanned((self.path, i))),
        }
    }

    fn take_char_literal(
        &mut self,
        start_idx: SourceIdx,
    ) -> Result<Spanned<Token>, Spanned<Error<'static>>> {
        match self
            .chars
            .next()
            .ok_or(Error::EofInStringLiteral.to_spanned((self.path, start_idx)))?
        {
            (i, '\\') => {
                let esc_char = self.parse_string_escape(i).unwrap_or(0x00); // error already handled, use 0x00 for placeholder
                let (end_idx, next_char) = self
                    .chars
                    .next()
                    .ok_or(Error::EofInStringLiteral.to_spanned((self.path, i)))?;
                if next_char == '\'' {
                    Ok(Token::CharLiteral(esc_char).to_spanned((self.path, start_idx, end_idx)))
                } else {
                    Err(Error::CharLiteralMissingEndQuote.to_spanned((self.path, end_idx)))
                }
            }
            (i, c) => {
                let (end_idx, next_char) = self
                    .chars
                    .next()
                    .ok_or(Error::EofInStringLiteral.to_spanned((self.path, i)))?;
                if next_char != '\'' {
                    return Err(Error::CharLiteralMissingEndQuote.to_spanned((self.path, end_idx)));
                }
                if c.is_ascii() {
                    let token = Token::CharLiteral(c as u8);
                    Ok(token.to_spanned((self.path, start_idx, end_idx)))
                } else {
                    Err(Error::NonAsciiCharLiteral.to_spanned((self.path, i)))
                }
            }
        }
    }

    fn take_str_literal(
        &mut self,
        start_idx: SourceIdx,
    ) -> Result<Spanned<Token>, Spanned<Error<'static>>> {
        let mut bytes = Vec::<u8>::new();
        let mut end_idx = start_idx;
        if self.chars.peek().is_none() {
            return Err(Error::EofInStringLiteral.to_spanned((self.path, start_idx)));
        }
        while let Some((current_idx, ch)) = self.chars.next_if(|&(_, c)| c != '\"') {
            if ch == '\\' {
                let esc_byte = self.parse_string_escape(current_idx)?;
                bytes.push(esc_byte);
            } else {
                bytes.extend_from_slice(ch.encode_utf8(&mut [0; 4]).as_bytes())
            }
        }
        if let Some((i, _)) = self.chars.next() {
            end_idx = i;
        }
        Ok(Token::StrLiteral(bytes.into()).to_spanned((self.path, start_idx, end_idx)))
    }

    fn parse_macro(&mut self, start_idx: SourceIdx) -> Result<(), Spanned<Error<'static>>> {
        let keyword_start = self
            .chars
            .peek()
            .ok_or(Error::UnexpectedEof.to_spanned((self.path, start_idx)))?
            .0;
        let mut keyword_end = self
            .chars
            .next_if(|(_, c)| c.is_alphanumeric())
            .ok_or(Error::ExpectIdentAfterHashtag.to_spanned((self.path, start_idx)))?
            .0;
        while let Some((i, _)) = self.chars.next_if(|(_, c)| c.is_alphanumeric()) {
            keyword_end = i;
        }
        let keyword = &self.source[keyword_start..keyword_end];
        match keyword.to_lowercase().as_str() {
            "if" => todo!(),
            "ifdef" => todo!(),
            "ifndef" => todo!(),
            "elif" => todo!(),
            "else" | "endif" => {
                Err(Error::InvalidPpKeyword.to_spanned((self.path, keyword_start, keyword_end)))
            }
            "include" => todo!(),
            "define" => {
                let token = self.next().ok_or(Error::UnexpectedEof.to_spanned((
                    self.path,
                    keyword_start,
                    keyword_end,
                )))?;
                let prev_span = token.span();
                let name = match_into!(token.into_inner(), Token::Ident(s) => s)
                    .ok_or(Error::ExpectIdent.to_spanned(prev_span))?;
                if self.chars.peek().is_some_and(|&(_, c)| c == '(') {
                    todo!();
                }
                while let Some((_, ch)) = self.chars.peek() {
                    match ch {
                        '\n' => {
                            self.chars.next();
                            self.macros.insert(name, None);
                            break;
                        }
                        '\\' => {
                            todo!()
                        }
                        ch if ch.is_whitespace() => {
                            self.chars.next();
                            continue;
                        }
                        _ => todo!(),
                    }
                }
                Ok(())
            }
            "undef" => todo!(),
            "line" => todo!("#line macro"),
            "error" => todo!(),
            "prgama" => todo!("#pragma"),
            _ => Err(Error::InvalidPpKeyword.to_spanned((self.path, keyword_start, keyword_end))),
        }
    }
}

impl Iterator for TokenStream {
    type Item = Spanned<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(token) = self.macro_buffer.pop() {
                return Some(token.to_spanned(self.current_macro_callsite));
            }
            macro_rules! next_and_return_some {
                ($x:expr) => {{
                    self.chars.next();
                    return Some($x);
                }};
            }
            break match self.chars.next()? {
                (_, c) if c.is_whitespace() => continue,

                (i, '#') => {
                    let _ = self
                        .parse_macro(i)
                        .inspect_err(|err| self.err_reporter.report(err));
                    continue;
                }

                (i, c) if c.is_alphabetic() || c == '_' => self.take_ident_or_keyword(i),
                (i, c) if c.is_numeric() => self.take_number(i, c),
                (i, '\'') => Some(
                    self.take_char_literal(i)
                        .inspect_err(|err| self.err_reporter.report(err))
                        .unwrap_or(Token::CharLiteral(0x00).to_spanned((self.path, i))),
                ),
                (i, '\"') => Some(
                    self.take_str_literal(i)
                        .inspect_err(|err| self.err_reporter.report(err))
                        .unwrap_or(Token::StrLiteral(Rc::new([])).to_spanned((self.path, i))),
                ),

                (i, '(') => Some(Token::ParenOpen.to_spanned((self.path, i))),
                (i, ')') => Some(Token::ParenClose.to_spanned((self.path, i))),
                (i, '[') => Some(Token::BracketOpen.to_spanned((self.path, i))),
                (i, ']') => Some(Token::BracketClose.to_spanned((self.path, i))),
                (i, '{') => Some(Token::BraceOpen.to_spanned((self.path, i))),
                (i, '}') => Some(Token::BraceClose.to_spanned((self.path, i))),

                (i, '+') => match self.chars.peek().clone() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::AddEq.to_spanned((self.path, i, i0)))
                    }
                    Some(&(i0, '+')) => {
                        next_and_return_some!(Token::AddAdd.to_spanned((self.path, i, i0)))
                    }
                    _ => Some(Token::Add.to_spanned((self.path, i))),
                },
                (i, '-') => match self.chars.peek() {
                    Some(&(i0, '>')) => {
                        next_and_return_some!(Token::Arrow.to_spanned((self.path, i, i0)))
                    }
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::SubEq.to_spanned((self.path, i, i0)))
                    }
                    Some(&(i0, '-')) => {
                        next_and_return_some!(Token::SubSub.to_spanned((self.path, i, i0)))
                    }
                    _ => Some(Token::Sub.to_spanned((self.path, i))),
                },
                (i, '*') => match self.chars.peek() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::MulEq.to_spanned((self.path, i, i0)))
                    }
                    _ => Some(Token::Mul.to_spanned((self.path, i))),
                },
                (i, '/') => match self.chars.peek() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::DivEq.to_spanned((self.path, i, i0)))
                    }
                    Some(&(_, '/')) => {
                        // Single line comment
                        while let Some((_, c)) = self.chars.next() {
                            if c == '\n' {
                                break;
                            }
                        }
                        continue;
                    }
                    Some(&(_, '*')) => {
                        // Multi-line comment
                        let mut level = 1usize;
                        while level != 0 {
                            match self.chars.next()?.1 {
                                '/' => {
                                    if self.chars.next()?.1 == '*' {
                                        level += 1;
                                    }
                                }
                                '*' => {
                                    if self.chars.next()?.1 == '/' {
                                        level -= 1;
                                    }
                                }
                                _ => (),
                            }
                        }
                        continue;
                    }
                    _ => Some(Token::Div.to_spanned((self.path, i))),
                },
                (i, '%') => match self.chars.peek() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::RemEq.to_spanned((self.path, i, i0)))
                    }
                    _ => Some(Token::Rem.to_spanned((self.path, i))),
                },
                (i, '<') => match self.chars.peek() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::LtEq.to_spanned((self.path, i, i0)))
                    }
                    Some(&(i0, '<')) => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some(&(i0, '=')) => {
                                next_and_return_some!(Token::LtLtEq.to_spanned((self.path, i, i0)))
                            }
                            _ => Some(Token::LtLt.to_spanned((self.path, i, i0))),
                        }
                    }
                    _ => Some(Token::Lt.to_spanned((self.path, i))),
                },
                (i, '>') => match self.chars.peek() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::GtEq.to_spanned((self.path, i, i0)))
                    }
                    Some(&(i0, '>')) => {
                        self.chars.next();
                        match self.chars.peek() {
                            Some(&(i0, '=')) => {
                                next_and_return_some!(Token::GtGtEq.to_spanned((self.path, i, i0)))
                            }
                            _ => Some(Token::GtGt.to_spanned((self.path, i, i0))),
                        }
                    }
                    _ => Some(Token::Gt.to_spanned((self.path, i))),
                },
                (i, '~') => Some(Token::Tilde.to_spanned((self.path, i))),
                (i, '!') => match self.chars.peek() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::ExcEq.to_spanned((self.path, i, i0)))
                    }
                    _ => Some(Token::Exc.to_spanned((self.path, i))),
                },
                (i, '|') => match self.chars.peek() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::OrEq.to_spanned((self.path, i, i0)))
                    }
                    Some(&(i0, '|')) => {
                        next_and_return_some!(Token::OrOr.to_spanned((self.path, i, i0)))
                    }
                    _ => Some(Token::Or.to_spanned((self.path, i))),
                },
                (i, '&') => match self.chars.peek() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::AndEq.to_spanned((self.path, i, i0)))
                    }
                    Some(&(i0, '&')) => {
                        next_and_return_some!(Token::AndAnd.to_spanned((self.path, i, i0)))
                    }
                    _ => Some(Token::And.to_spanned((self.path, i))),
                },
                (i, '^') => match self.chars.peek() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::XorEq.to_spanned((self.path, i, i0)))
                    }
                    _ => Some(Token::Xor.to_spanned((self.path, i))),
                },
                (i, '=') => match self.chars.peek() {
                    Some(&(i0, '=')) => {
                        next_and_return_some!(Token::EqEq.to_spanned((self.path, i, i0)))
                    }
                    _ => Some(Token::Eq.to_spanned((self.path, i))),
                },
                (i, '.') => Some(Token::Dot.to_spanned((self.path, i))),
                (i, ',') => Some(Token::Comma.to_spanned((self.path, i))),
                (i, ':') => Some(Token::Colon.to_spanned((self.path, i))),
                (i, ';') => Some(Token::Semicolon.to_spanned((self.path, i))),
                (i, '?') => Some(Token::Ques.to_spanned((self.path, i))),
                (i, '\\') => Some(Token::Backslash.to_spanned((self.path, i))),
                (i, c) => {
                    self.err_reporter
                        .report(&Error::InvalidCharacter(c).to_spanned((self.path, i)));
                    continue;
                }
            };
        }
    }
}
