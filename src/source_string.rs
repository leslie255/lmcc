#![allow(dead_code)]

use std::{
    fmt::{self, Debug, Display},
    mem,
    ops::{Index, Range},
    slice,
    str::{self, Chars},
};

/// An immutable string type that allows O(1) indexing using `SourceIdx`.
/// `SourceIdx` can only be obtained by `SourceCharIndices::next`.
///
/// # Examples
///
/// ```
/// let string = SourceString::new("你好，世界\n🌮\nПривет, мир\n");
///
/// let index0 = string.char_indices().skip(8).next().unwrap().0;
/// let index1 = string.char_indices().skip(13).next().unwrap().0;
/// assert_eq!(string.slice(index0..index1), Some("Привет"));
/// // Alternatively, use `SourceString::slice` to slice a `SourceString` and panic when index out
/// // out of range.
/// assert_eq!(&string[index0..index1], "Привет");
/// ```
#[repr(transparent)]
#[derive(PartialEq, Eq)]
pub struct SourceString(str);

impl<'a> From<&'a str> for &'a SourceString {
    fn from(value: &'a str) -> Self {
        SourceString::new(value)
    }
}

impl SourceString {
    /// Returns an iterator of the characters.
    ///
    /// # Examples
    ///
    /// ```
    /// let string = SourceString::new("你好，世界\n🌮\nПривет, мир\n");
    /// let mut iter = string.chars();
    /// assert_eq!(iter.next(), Some('你'));
    /// assert_eq!(iter.next(), Some('好'));
    /// ```
    pub fn chars(&self) -> SourceChars {
        SourceChars {
            iter: self.0.chars(),
        }
    }

    /// Returns an iterator of the character and their indices.
    ///
    /// # Examples
    ///
    /// ```
    /// let string = SourceString::new("你好，世界\n🌮\nПривет, мир\n");
    /// let mut iter = string.char_indices();
    /// let (index0, char) = iter.next().unwrap();
    /// assert_eq!(char, '你');
    /// let (index1, char) = iter.next().unwrap();
    /// assert_eq!(char, '好');
    /// let nihao = &string[index0..index1];
    /// assert_eq!(nihao, "你好");
    /// ```
    pub fn char_indices(&self) -> SourceCharIndices {
        SourceCharIndices {
            iter: self.as_bytes().iter(),
            i: 0,
            raw_index: 0,
        }
    }

    /// Returns an iterator of the lines in this string (including linebreak character, along with
    /// their start and end indices.
    /// Start index equals end index if the line has only one character.
    pub fn lines_indexed(&self) -> SourceLinesIndexed {
        SourceLinesIndexed {
            s: self,
            char_indices: self.char_indices(),
        }
    }

    /// Slice the string into a `&str`.
    /// Returns `None` if index is out of range.
    ///
    /// ```
    /// let string = SourceString::new("你好，世界\n🌮\nПривет, мир\n");
    ///
    /// let index0 = string.char_indices().skip(8).next().unwrap().0;
    /// let index1 = string.char_indices().skip(13).next().unwrap().0;
    ///
    /// assert_eq!(string.slice(index0..index1), Some("Привет"));
    /// ```
    #[inline]
    pub fn slice(&self, index: Range<SourceIdx>) -> Option<&str> {
        let bytes = self
            .as_bytes()
            .get(index.start.raw..index.end.raw + index.end.len)?;
        unsafe { Some(str::from_utf8_unchecked(bytes)) }
    }
}

impl Debug for SourceString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.0, f)
    }
}

impl Display for SourceString {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl SourceString {
    #[inline]
    pub const fn as_str(&self) -> &str {
        &self.0
    }

    #[inline]
    pub const fn as_bytes(&self) -> &[u8] {
        self.0.as_bytes()
    }

    #[inline]
    pub const fn new(s: &str) -> &Self {
        unsafe { mem::transmute(s) }
    }
}

impl Index<Range<SourceIdx>> for SourceString {
    type Output = str;

    fn index(&self, index: Range<SourceIdx>) -> &Self::Output {
        let bytes = self
            .as_bytes()
            .get(index.start.raw..index.end.raw + index.end.len);
        let bytes = match bytes {
            Some(bytes) => bytes,
            None => {
                panic!("Index out of range when slicing a `SourceString`");
            }
        };
        unsafe { str::from_utf8_unchecked(bytes) }
    }
}

/// An index pointing to a raw position in the source string.
///
/// # Safety
/// Index produced from one source can never be used in another source string.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SourceIdx {
    /// Raw index of the byte.
    pub raw: usize,
    /// Length of the character (in bytes).
    pub len: usize,
    /// The index of the character in the string.
    pub utf8_pos: usize,
}

impl SourceIdx {
    pub unsafe fn new(raw: usize, len: usize, utf8position: usize) -> Self {
        Self {
            raw,
            len,
            utf8_pos: utf8position,
        }
    }
}

const CONT_MASK: u8 = 0b0011_1111;

#[inline]
const fn utf8_first_byte(byte: u8, width: u32) -> u32 {
    (byte & (0x7F >> width)) as u32
}

/// Returns the value of `ch` updated with continuation byte `byte`.
#[inline]
const fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

/// Modified from `core::str::next_code_point`.
/// Returns the length of the character, and the next code point in UTF-8.
unsafe fn next_code_point_indexed<'a, I: Iterator<Item = &'a u8>>(
    bytes: &mut I,
) -> Option<(usize, u32)> {
    // Decode UTF-8.
    let x = *bytes.next()?;
    if x < 128 {
        return Some((1, x as u32));
    }

    // Multibyte case follows.
    // Decode from a byte combination out of: [[[x y] z] w].
    // NOTE: Performance is sensitive to the exact formulation here.
    let init = utf8_first_byte(x, 2);
    // SAFETY: `bytes` produces an UTF-8-like string,
    // so the iterator must produce a value here.
    let y = *bytes.next().unwrap_unchecked();
    let mut increments = 2usize;
    let mut ch = utf8_acc_cont_byte(init, y);
    if x >= 0xE0 {
        // [[x y z] w] case.
        // 5th bit in 0xE0 .. 0xEF is always clear, so `init` is still valid.
        // SAFETY: `bytes` produces an UTF-8-like string,.
        // so the iterator must produce a value here..
        let z = *bytes.next().unwrap_unchecked();
        increments = 3;
        let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
        ch = init << 12 | y_z;
        if x >= 0xF0 {
            // [x y z w] case.
            // Use only the lower 3 bits of `init`.
            // SAFETY: `bytes` produces an UTF-8-like string,
            // so the iterator must produce a value here.
            let w = *bytes.next().unwrap_unchecked();
            increments = 4;
            ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
        }
    }

    Some((increments, ch))
}

/// See `SourceString::lines_indices`.
#[derive(Clone, Debug)]
pub struct SourceCharIndices<'a> {
    iter: slice::Iter<'a, u8>,
    i: usize,
    raw_index: usize,
}

impl Iterator for SourceCharIndices<'_> {
    type Item = (SourceIdx, char);

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            let (len, ch) = next_code_point_indexed(&mut self.iter)?;
            let raw_index = self.raw_index;
            self.raw_index += len;
            let position = self.i;
            self.i += 1;
            let ch = char::from_u32_unchecked(ch);

            Some((
                SourceIdx {
                    raw: raw_index,
                    len,
                    utf8_pos: position,
                },
                ch,
            ))
        }
    }
}

/// See `SourceString::lines_indexed`
#[derive(Clone, Debug)]
pub struct SourceLinesIndexed<'a> {
    s: &'a SourceString,
    char_indices: SourceCharIndices<'a>,
}

impl<'a> Iterator for SourceLinesIndexed<'a> {
    type Item = (SourceIdx, SourceIdx, &'a str);

    fn next(&mut self) -> Option<Self::Item> {
        let start_idx = match self.char_indices.next()? {
            (idx, '\n') => return Some((idx, idx, "\n")),
            (idx, _) => idx,
        };
        let mut end_idx = start_idx;
        loop {
            match self.char_indices.next() {
                Some((idx, '\n')) => break Some((start_idx, idx, &self.s[start_idx..idx])),
                Some((idx, _)) => {
                    end_idx = idx;
                }
                None => break Some((start_idx, end_idx, &self.s[start_idx..end_idx])),
            }
        }
    }
}

/// See `SourceString::chars`
#[derive(Clone, Debug)]
pub struct SourceChars<'a> {
    iter: Chars<'a>,
}

impl Iterator for SourceChars<'_> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}
