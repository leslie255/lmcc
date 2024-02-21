#![allow(dead_code)]
use std::{
    borrow::Cow,
    fmt::{self, Debug, Display},
    ops::{Deref, DerefMut},
    process,
    rc::Rc,
    sync::atomic::{self, AtomicU32},
};

use unicode_width::UnicodeWidthChar;

use crate::{
    ast::Ty, file_reader::FileReader, source_string::SourceIdx, token::Token, utils::{derive_display_from_debug, fixme, IdentStr}
};

pub type CowStr = Cow<'static, str>;

#[derive(Clone, Copy, Debug)]
pub enum ExprNotAllowedReason {
    InvalidTopLevelStmt,
    NestedFunc,
    StmtAsExpr,
    NoReason,
}

#[derive(Clone, Debug)]
pub enum Error<'a> {
    Todo(&'a str),
    /// Unlike `Todo`, the string have to be one sentence.
    /// e.g. "_Complex is not supported"
    Unsupported(&'a str),
    InvalidNumberFormat,
    EofInStringLiteral,
    InvalidStringEscape,
    CharLiteralMissingEndQuote,
    NonAsciiCharLiteral,
    InvalidCharacter(char),
    UnexpectedEof,
    InvalidPpKeyword,
    ExpectIdentAfterHashtag,
    ExpectIdent,
    ExpectNumLiteral,
    ExpectCharLiteral,
    ExpectStrLiteral,
    ExpectToken(Token),
    ExpectTokens(&'a [Token]),
    UnexpectedToken(Token),
    RestrictOnNonPointer,
    ConflictingSignness,
    InvalidSignnessFlag,
    ExpectTy,
    InvalidStorageClass,
    ConflictingStorageClass,
    DuplicateSpecifier,
    ConflictingTypeSpecifier,
    LongLongLongIsTooLong,
    InlineInVarDecl,
    IllegalArrLen,
    ArrAsFuncRet,
    MissingSemicolon,
    NonDeclInStructUnion,
    StorageClassInStructUnion,
    RhsInStructUnion,
    AnonStructUnion,
    AnonEnum,
    ExprNoAssignable,
    VarDoesNotExist(IdentStr),
    OutOfRangeNumber(i128),
    ExprNotAllowed(ExprNotAllowedReason),
    RedefinitionOfVar(IdentStr),
    NonNumericInUnary,
    TypedefWithRhs,
    FuncDoesNotExist(IdentStr),
    MismatchedArgCount(usize, usize),
    MismatchedType(&'a Ty, &'a Ty),
}

impl Error<'_> {
    fn description<'a>(&'a self) -> ErrorDescription<'a> {
        ErrorDescription(self)
    }
}

/// A value with attached span information.
///
/// When using `PartialEq` and `PartialOrd` on `Spanned<T>`, but only the value itself would be compared, not the attached span.
///
/// `Spanned<T>` isn't too different from two separate `T` and `Span`, it's only there to help clarify the intent of the code.
/// e.g. A function that returns `Spanned<IdentStr>` is foundamentally not much different from a function that returns `(IdentStr, Span)`,
/// but the former communicates the idea that the returned `Span` is the span of the identifier token, whereas the latter may require some explanation in its doc.
#[derive(Clone, Copy)]
pub struct Spanned<T> {
    inner: T,
    span: Span,
}
impl<T, U> PartialEq<Spanned<U>> for Spanned<T>
where
    T: PartialEq<U>,
{
    fn eq(&self, other: &Spanned<U>) -> bool {
        self.inner() == other.inner()
    }
    fn ne(&self, other: &Spanned<U>) -> bool {
        self.inner() != other.inner()
    }
}
impl<T, U> PartialOrd<Spanned<U>> for Spanned<T>
where
    T: PartialOrd<U>,
{
    fn partial_cmp(&self, other: &Spanned<U>) -> Option<std::cmp::Ordering> {
        self.inner().partial_cmp(other.inner())
    }
    fn lt(&self, other: &Spanned<U>) -> bool {
        self.inner() < other.inner()
    }
    fn le(&self, other: &Spanned<U>) -> bool {
        self.inner() <= other.inner()
    }
    fn gt(&self, other: &Spanned<U>) -> bool {
        self.inner() > other.inner()
    }
    fn ge(&self, other: &Spanned<U>) -> bool {
        self.inner() >= other.inner()
    }
}
impl<T> Debug for Spanned<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.inner(), f)
    }
}
impl<T> Display for Spanned<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self.inner(), f)
    }
}
impl<T> Spanned<Box<T>> {
    pub fn into_unboxed(self) -> Spanned<T> {
        self.map(|x| *x)
    }
}

impl<T> Spanned<T> {
    pub fn into_boxed(self) -> Spanned<Box<T>> {
        self.map(Box::new)
    }

    pub const fn span(&self) -> Span {
        self.span
    }

    pub fn span_mut(&mut self) -> &mut Span {
        &mut self.span
    }

    pub const fn inner(&self) -> &T {
        &self.inner
    }

    pub fn into_inner(self) -> T {
        self.inner
    }

    pub fn into_pair(self) -> (T, Span) {
        (self.inner, self.span)
    }

    pub const fn as_pair(&self) -> (&T, Span) {
        (&self.inner, self.span)
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        let span = self.span();
        Spanned {
            inner: f(self.into_inner()),
            span,
        }
    }

    pub fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Spanned<U>, E> {
        let span = self.span();
        Ok(Spanned {
            inner: f(self.into_inner())?,
            span,
        })
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq)]
pub struct Span {
    pub file: &'static str,
    pub start: usize,
    pub end: usize,
}

derive_display_from_debug!(Span);
impl Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}~#{}@{:?}", self.start, self.end, self.file)
    }
}

impl Span {
    pub const fn join(self, other: Self) -> Self {
        Self {
            file: self.file,
            start: self.start,
            end: other.end,
        }
    }
    pub const fn tail(self) -> Self {
        Self {
            file: self.file,
            start: self.end,
            end: self.end,
        }
    }
    pub const fn head(self) -> Self {
        Self {
            file: self.file,
            start: self.start,
            end: self.start,
        }
    }
}

impl From<(&'static str, SourceIdx, SourceIdx)> for Span {
    fn from(x: (&'static str, SourceIdx, SourceIdx)) -> Self {
        Self {
            file: x.0,
            start: x.1.utf8_pos,
            end: x.2.utf8_pos,
        }
    }
}

impl From<(&'static str, SourceIdx)> for Span {
    fn from(x: (&'static str, SourceIdx)) -> Self {
        Self {
            file: x.0,
            start: x.1.utf8_pos,
            end: x.1.utf8_pos,
        }
    }
}

impl From<(&'static str, usize, usize)> for Span {
    fn from(x: (&'static str, usize, usize)) -> Self {
        Self {
            file: x.0,
            start: x.1,
            end: x.2,
        }
    }
}

impl From<(&'static str, usize)> for Span {
    fn from(x: (&'static str, usize)) -> Self {
        Self {
            file: x.0,
            start: x.1,
            end: x.1,
        }
    }
}

pub trait ToSpanned {
    fn to_spanned(self, span: impl Into<Span>) -> Spanned<Self>
    where
        Self: Sized;
}

impl<T> ToSpanned for T {
    fn to_spanned(self, span: impl Into<Span>) -> Spanned<Self> {
        Spanned {
            inner: self,
            span: span.into(),
        }
    }
}

/// Display something in the source file, such as function names, in quote style.
fn quote<T: Display>(x: T) -> Quote<T> {
    Quote(x)
}
#[derive(Debug, Clone, Copy)]
struct Quote<T: Display>(T);
impl<T: Display> Display for Quote<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "`{}`", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
struct ErrorDescription<'a>(&'a Error<'a>);
impl Display for ErrorDescription<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", &self.0)
    }
}

#[derive(Clone, Debug)]
pub struct ErrorFormatter<'a, 'f> {
    sources: &'f FileReader,
    error: &'a Spanned<Error<'a>>,
}

impl<'a, 'f> ErrorFormatter<'a, 'f> {
    pub const fn new(sources: &'f FileReader, error: &'a Spanned<Error>) -> Self {
        Self { sources, error }
    }
}

impl<'a, 'f> Display for ErrorFormatter<'a, 'f> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let error_span = self.error.span();
        macro_rules! invalid_error_span {
            () => {{
                return writeln!(
                    f,
                    "error @ invalid span {:?}:\n{:?}",
                    error_span,
                    self.error.description()
                );
            }};
        }
        let Some(source) = self.sources.read_file(error_span.file) else {
            invalid_error_span!()
        };
        let Some(error_span_len) = error_span.end.checked_sub(error_span.start) else {
            invalid_error_span!()
        };

        // Find lines to display and line/column number.
        // Only display the first and last line if there are more than 2 lines.
        let mut line0 = "";
        // let mut line1
        let mut line_count = 0u32; // number of lines this error span across
        let mut col_idx = 0u32;
        let mut row_idx = 0u32;
        for (i, (line_start, line_end, line)) in source.lines_indexed().enumerate() {
            if line_start.utf8_pos <= error_span.start && error_span.start <= line_end.utf8_pos {
                line0 = line;
                if line_end.utf8_pos >= error_span.end {
                    line_count = 1;
                } else {
                    fixme!("Format multi-line errors");
                    line_count = 2;
                }
                row_idx = i as u32;
                col_idx = (error_span.start - line_start.utf8_pos) as u32;
                break;
            }
        }

        if line_count == 0 {
            invalid_error_span!();
        }

        // Line number and error description.
        write!(
            f,
            "error @ {}:{}:{}:\n{}\n\n",
            error_span.file,
            row_idx,
            col_idx,
            self.error.description(),
        )?;

        // Print the line.
        for char in line0.chars() {
            match char {
                '\t' => write!(f, "    ")?,
                '\n' => break,
                c => write!(f, "{}", c)?,
            }
        }

        println!();

        // Tick pointing to the error spot in line.
        let mut chars = line0.chars();
        for _ in 0..col_idx {
            let char = chars.next().unwrap();
            match char {
                '\t' => write!(f, "    ")?,
                c => {
                    for _ in 0..c.width().unwrap_or(0) {
                        write!(f, " ")?;
                    }
                }
            }
        }

        write!(f, "^")?;
        for _ in (0..error_span_len).zip(line0.chars()) {
            write!(f, "~")?;
        }
        writeln!(f)
    }
}

#[derive(Clone, Debug)]
pub struct UnspannedErrorFormatter<'a> {
    error: &'a Error<'a>,
}

impl<'a> UnspannedErrorFormatter<'a> {
    pub const fn new(error: &'a Error) -> Self {
        Self { error }
    }
}

impl<'a> Display for UnspannedErrorFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "error @ unknown position: {}", self.error.description())
    }
}

#[derive(Debug)]
pub struct ErrorReporter {
    file_reader: Rc<FileReader>,
    error_count: AtomicU32,
}

impl ErrorReporter {
    pub fn new(file_reader: Rc<FileReader>) -> Rc<Self> {
        Self {
            file_reader,
            error_count: 0.into(),
        }
        .into()
    }

    pub fn report(&self, err: &Spanned<Error>) {
        self.error_count.fetch_add(1, atomic::Ordering::SeqCst);
        print!("{}", ErrorFormatter::new(&self.file_reader, err))
    }

    #[allow(dead_code)]
    pub fn report_spanless(&self, err: Error) {
        self.error_count.fetch_add(1, atomic::Ordering::SeqCst);
        print!("{}", UnspannedErrorFormatter::new(&err))
    }

    pub fn error_count(&self) -> u32 {
        self.error_count.load(atomic::Ordering::SeqCst)
    }

    /// Exits the program with a message if at least one error was previously reported.
    pub fn exit_if_has_error(&self) {
        match self.error_count() {
            0 => (),
            1 => {
                println!("Cannot compile program due to this error");
                process::exit(1);
            }
            err_count => {
                println!(
                    "Cannot compile program due to {} previous errors",
                    err_count
                );
                process::exit(1);
            }
        }
    }
}
