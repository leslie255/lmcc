#![allow(dead_code)]

use std::{
    fmt::{self, Debug, Display},
    mem, option, slice,
};

use index_vec::IndexVec;

use crate::intern_str::InternStr;

pub type IdentStr = InternStr<'static>;

pub macro match_into($val:expr, $pat:pat => $result:expr) {
    match $val {
        $pat => Some($result),
        #[allow(unreachable_patterns)]
        _ => None,
    }
}

pub macro match_into_unchecked($val:expr, $pat:pat => $result:expr) {
    match $val {
        $pat => $result,
        #[allow(unreachable_patterns)]
        _ => std::hint::unreachable_unchecked(),
    }
}

pub trait OptionTryExtension<T> {
    fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Option<U>, E>;
    fn try_map_or<U, E>(self, default: U, f: impl FnOnce(T) -> Result<U, E>) -> Result<U, E>;
    fn try_map_or_else<U, E>(
        self,
        f: impl FnOnce() -> U,
        g: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<U, E>;
    fn try_map_or_try_else<U, E>(
        self,
        f: impl FnOnce() -> Result<U, E>,
        g: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<U, E>;
    fn try_inspect<E>(self, f: impl FnOnce(T) -> Result<(), E>) -> Result<(), E>;
    fn unwrap_or_try_else<E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<T, E>;
}

impl<T> OptionTryExtension<T> for Option<T> {
    fn try_map<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Option<U>, E> {
        match self {
            Some(x) => f(x).map(|x| Some(x)),
            None => Ok(None),
        }
    }

    fn try_map_or<U, E>(self, default: U, f: impl FnOnce(T) -> Result<U, E>) -> Result<U, E> {
        match self {
            Some(x) => f(x),
            None => Ok(default),
        }
    }

    fn try_map_or_else<U, E>(
        self,
        f: impl FnOnce() -> U,
        g: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<U, E> {
        match self {
            Some(x) => g(x),
            None => Ok(f()),
        }
    }

    fn try_map_or_try_else<U, E>(
        self,
        f: impl FnOnce() -> Result<U, E>,
        g: impl FnOnce(T) -> Result<U, E>,
    ) -> Result<U, E> {
        match self {
            Some(x) => g(x),
            None => f(),
        }
    }

    fn try_inspect<E>(self, f: impl FnOnce(T) -> Result<(), E>) -> Result<(), E> {
        match self {
            Some(x) => f(x),
            None => Ok(()),
        }
    }

    fn unwrap_or_try_else<E>(self, f: impl FnOnce() -> Result<T, E>) -> Result<T, E> {
        match self {
            Some(x) => Ok(x),
            None => f(),
        }
    }
}

/// Const promotion for expressions that can't be implicitly const-promoted, such as calling a
/// `const fn`.
/// Requires providing an explicit type due to details in the syntax.
/// Meant to be used like `const_promote!(T, const_fn())`.
pub macro const_promote($ty:ty, $value:expr) {
    static VALUE: $ty = $value;
    &VALUE
}

/// Prints a FIXME message with file name and line number similar to `todo!`, but does not panic or
/// exit.
pub macro fixme {
    () => {{
        println!("[{}:{}] FIXME", file!(), line!());
    }},
    ($($args:tt)*) => {{
        print!("[{}:{}] FIXME ", file!(), line!());
        println!($($args)*);
    }},
}

/// An iterator that can contain either `&T` or `&[T]`.
/// If it contains a `&T`, the iterator has only one item.
/// If it contains a `&[T]`, the iterator behaves like a normal slice iterator.
/// Can be constructed by `MaybeMultiple::from`.
#[derive(Debug, Clone)]
pub enum MaybeMultiple<'a, T> {
    One(option::IntoIter<&'a T>),
    More(slice::Iter<'a, T>),
}

impl<'a, T> Iterator for MaybeMultiple<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::One(x) => x.next(),
            Self::More(x) => x.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            MaybeMultiple::One(x) => x.size_hint(),
            MaybeMultiple::More(x) => x.size_hint(),
        }
    }
}

impl<'a, T> From<&'a T> for MaybeMultiple<'a, T> {
    fn from(value: &'a T) -> Self {
        Self::One(Some(value).into_iter())
    }
}

impl<'a, T> From<&'a [T]> for MaybeMultiple<'a, T> {
    fn from(value: &'a [T]) -> Self {
        Self::More(value.iter())
    }
}

impl<'a, T> MaybeMultiple<'a, T> {
    pub fn none() -> Self {
        Self::One(None.into_iter())
    }

    pub fn one(x: &'a T) -> Self {
        Self::One(Some(x).into_iter())
    }

    pub fn more(x: &'a [T]) -> Self {
        Self::More(x.iter())
    }
}

#[inline(always)]
pub fn ptr_addr<T>(ptr: *const T) -> usize {
    unsafe { mem::transmute(ptr) }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct IndexVecKVPairs<I: Debug + index_vec::Idx, T: Debug>(IndexVec<I, T>);
impl<I: Debug + index_vec::Idx, T: Debug> Debug for IndexVecKVPairs<I, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.0.iter_enumerated()).finish()
    }
}

pub fn index_vec_kv_pairs<I: Debug + index_vec::Idx, T: Debug>(
    index_vec: &IndexVec<I, T>,
) -> &IndexVecKVPairs<I, T> {
    unsafe { std::mem::transmute(index_vec) }
}

#[derive(Clone, Copy)]
pub struct SliceKVPairs<'a, T: Debug>(pub &'a [T]);

impl<'a, T: Debug> Debug for SliceKVPairs<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.0.iter().enumerate()).finish()
    }
}

#[derive(Clone, Copy)]
pub struct FmtRepeat {
    content: &'static str,
    times: u32,
}
impl Debug for FmtRepeat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for _ in 0..self.times {
            write!(f, "{}", self.content)?;
        }
        Ok(())
    }
}
impl Display for FmtRepeat {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self, f)
    }
}

#[inline(always)]
pub fn fmt_repeat(content: &'static str, times: u32) -> FmtRepeat {
    FmtRepeat { content, times }
}

#[inline(always)]
pub fn fmt_indents(times: u32) -> FmtRepeat {
    fmt_repeat("  ", times)
}

/// Note that this only works for cases without `where` clause.
pub macro derive_display_from_debug($T:ty) {
    impl std::fmt::Display for $T {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            std::fmt::Debug::fmt(self, f)
        }
    }
}

/// Unlike `Result::unwrap_or_else`, this macro allows adding a diverging statement (`return`, `break`, `continue`) on the error case.
/// (Thus requiring the use of macros over regular functions).
///
/// ```
/// let unwrapped = result_unwrap_or!(x, |e| println!("{e:?}"), return);
/// ```
///
/// One limitation (compared with manual `match` statement) is that the statement in the (third argument) cannot make use of the error value.
pub macro result_unwrap_or($self:expr , $err_handler:expr , $otherwise:stmt $(,)?) {
    match $self {
        Ok(x) => x,
        Err(e) => {
            $err_handler(e);
            $otherwise
        }
    }
}

pub trait DoInBetween {
    /// Executes `f` in-between two elements, executes `g` for every element.
    /// Useful for the common "insert separator between elements" pattern.
    /// Offers a `captured` for (effectively) mut capturing a value in both `f` and `g`, which is
    /// forbidden by the borrow checker.
    ///
    /// # Examples
    ///
    /// ```rust
    /// let vec = vec![10i32, 4, 5, 7, 100];
    /// let mut count = 0usize;
    /// // say we have to increment count for every separator, for some reason
    /// vec.iter().do_in_between(
    ///     &mut count,
    ///     |count| {
    ///         print!(",");
    ///         *count += 1;
    ///     },
    ///     |count, item| {
    ///         print!("{item}");
    ///         *count += 1;
    ///     },
    /// );
    /// ```
    fn do_in_between<C, F, G>(self, captured: C, f: F, g: G) -> C
    where
        Self: Iterator + Sized,
        F: FnMut(&mut C),
        G: FnMut(&mut C, <Self as Iterator>::Item);

    /// See `do_in_between`.
    /// Allows returning an error in `f` or `g`, but they must be of the same type.
    /// Stops when an error is reached, returns the error.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use std::io::Write;
    /// use std::fs::File;
    /// let vec = vec![10i32, 4, 5, 7, 100];
    /// let mut file = File::open("file.txt").unwrap();
    /// let write_result = vec.iter().try_do_in_between(
    ///     &mut file,
    ///     |file| write!(file, ","),
    ///     |file, item| write!(file, "{item}"),
    /// );
    /// if let Err(e) = write_result {
    ///     println!("Error writing to the file: {e:?}")
    /// }
    /// ```
    fn try_do_in_between<C, F, G, E>(self, captured: C, f: F, g: G) -> Result<C, E>
    where
        Self: Iterator + Sized,
        F: FnMut(&mut C) -> Result<(), E>,
        G: FnMut(&mut C, <Self as Iterator>::Item) -> Result<(), E>;
}

impl<I: Iterator + Sized> DoInBetween for I {
    fn do_in_between<C, F, G>(mut self, mut captured: C, mut f: F, mut g: G) -> C
    where
        Self: Iterator + Sized,
        F: FnMut(&mut C),
        G: FnMut(&mut C, <Self as Iterator>::Item),
    {
        if let Some(first) = self.next() {
            g(&mut captured, first);
        } else {
            return captured;
        }
        while let Some(item) = self.next() {
            f(&mut captured);
            g(&mut captured, item);
        }
        captured
    }

    fn try_do_in_between<C, F, G, E>(mut self, mut captured: C, mut f: F, mut g: G) -> Result<C, E>
    where
        Self: Iterator + Sized,
        F: FnMut(&mut C) -> Result<(), E>,
        G: FnMut(&mut C, <Self as Iterator>::Item) -> Result<(), E>,
    {
        if let Some(first) = self.next() {
            g(&mut captured, first)?;
        } else {
            return Ok(captured);
        }
        while let Some(item) = self.next() {
            f(&mut captured)?;
            g(&mut captured, item)?;
        }
        Ok(captured)
    }
}
