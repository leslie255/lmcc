#![allow(dead_code)]

use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display};
use std::hash::Hash;
use std::ops::Deref;

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct InternStr<'a>(&'a str);
impl<'a> Deref for InternStr<'a> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}
impl<'a> AsRef<str> for InternStr<'a> {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}
impl<'a> Debug for InternStr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}
impl<'a> Display for InternStr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}
impl<'a> PartialEq for InternStr<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}
impl<'a> Eq for InternStr<'a> {}
impl<'a> InternStr<'a> {
    pub const fn as_str(self) -> &'a str {
        self.0
    }
}
impl Hash for InternStr<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.as_ptr().hash(state);
    }
}

/// Like `Cow<'static, str>`, except it uses `Box<str>` for the owned string.
#[derive(Clone, Debug)]
pub enum MaybeOwnedStr<'a> {
    Borrowed(&'a str),
    Owned(Box<str>),
}
impl<'a> MaybeOwnedStr<'a> {
    const fn as_str<'c, 'b>(&'b self) -> &'c str
    where
        'a: 'c,
        'b: 'c,
    {
        match self {
            MaybeOwnedStr::Borrowed(s) => s,
            MaybeOwnedStr::Owned(s) => s,
        }
    }
}
impl<'a> From<&'a str> for MaybeOwnedStr<'a> {
    fn from(value: &'a str) -> Self {
        Self::Borrowed(value)
    }
}
impl From<Box<str>> for MaybeOwnedStr<'_> {
    fn from(value: Box<str>) -> Self {
        Self::Owned(value)
    }
}
impl From<String> for MaybeOwnedStr<'_> {
    fn from(value: String) -> Self {
        Self::Owned(value.into_boxed_str())
    }
}

/// It's not `Sync`.
#[derive(Debug, Default)]
pub struct StringArena<'s> {
    strings: UnsafeCell<Vec<MaybeOwnedStr<'s>>>,
    indices: UnsafeCell<HashMap<*const str, usize>>,
}

unsafe impl<'s> Send for StringArena<'s> {}

impl<'s> StringArena<'s> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn intern(& self, s: impl Into<MaybeOwnedStr<'s>> + AsRef<str>) -> InternStr<> {
        let strings = unsafe { self.strings_mut() };
        let indices = unsafe { self.indices_mut() };
        match indices.get(s.as_ref()) {
            Some(&i) => {
                let s: &MaybeOwnedStr<'s> = unsafe { strings.get_unchecked(i) };
                InternStr(s.as_str())
            }
            None => {
                let i = strings.len();
                strings.push(s.into());
                let s: &MaybeOwnedStr<'s> = unsafe { strings.get_unchecked(i) };
                indices.insert(s.as_str(), i);
                InternStr(s.as_str())
            }
        }
    }

    #[allow(dead_code)]
    #[inline(always)]
    unsafe fn indices<'a>(&'a self) -> &'a HashMap<&'a str, usize> {
        std::mem::transmute(&*self.indices.get())
    }

    #[allow(dead_code)]
    #[inline(always)]
    unsafe fn indices_mut<'a>(&'a self) -> &'a mut HashMap<&'a str, usize> {
        std::mem::transmute(&mut *self.indices.get())
    }

    #[allow(dead_code)]
    #[inline(always)]
    unsafe fn strings<'a>(&'a self) -> &'a Vec<MaybeOwnedStr<'s>> {
        &*self.strings.get()
    }

    #[allow(dead_code)]
    #[inline(always)]
    unsafe fn strings_mut<'a>(&'a self) -> &'a mut Vec<MaybeOwnedStr<'s>> {
        &mut *self.strings.get()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ptr_addr<T>(ptr: *const T) -> usize {
        unsafe { std::mem::transmute(ptr) }
    }
    /// Like assert_eq, but prints the pointer addresses in hex on assert fail.
    macro assert_ptr_eq($lhs:expr, $rhs:expr) {{
        // Prevent lhs and rhs from being evaluated twice.
        let lhs = $lhs as *const _;
        let rhs = $rhs as *const _;
        assert!(
            lhs == rhs,
            "lhs: 0x{:016X}, rhs: 0x{:016X}",
            ptr_addr(lhs),
            ptr_addr(rhs),
        );
    }}
    /// Like assert_ne, but prints the pointer addresses in hex on assert fail.
    macro assert_ptr_ne($lhs:expr, $rhs:expr) {{
        // Prevent lhs and rhs from being evaluated twice.
        let lhs = $lhs as *const _;
        let rhs = $rhs as *const _;
        assert!(
            lhs != rhs,
            "lhs: 0x{:016X}, rhs: 0x{:016X}",
            ptr_addr(lhs),
            ptr_addr(rhs),
        );
    }}

    #[test]
    fn intern_str() {
        let arena = StringArena::new();

        assert_ptr_eq!(
            arena.intern(Box::from("hello, world")).as_ptr(),
            arena.intern("hello, world").as_ptr()
        );

        let s0 = arena.intern("hello, world");
        let s1 = arena.intern("你好，世界");
        let s2 = arena.intern(Box::from("привет, мир"));
        assert_ptr_eq!(s0.as_ptr(), s0.as_ptr());
        assert_ptr_ne!(s0.as_ptr(), s1.as_ptr());
        assert_ptr_ne!(s0.as_ptr(), s2.as_ptr());
        assert_ptr_eq!(s1.as_ptr(), s1.as_ptr());
        assert_ptr_ne!(s1.as_ptr(), s2.as_ptr());
        assert_ptr_eq!(s2.as_ptr(), s2.as_ptr());
        assert_eq!(&s0[..], "hello, world");
        assert_eq!(&s1[..], "你好，世界");
        assert_eq!(&s2[..], "привет, мир");

        let arena = StringArena::new();
        let s3 = arena.intern(Box::from("hello, world"));
        assert_ptr_ne!(s2.as_ptr(), s3.as_ptr());
    }
}
