use std::{cell::RefCell, collections::HashMap, fs, rc::Rc};

use crate::source_string::SourceString;

/// Manages opened files to avoid re-opening and reading the same file.
#[derive(Debug)]
pub struct FileReader {
    /// Map from path to file content as string.
    opened_files: RefCell<HashMap<&'static str, &'static SourceString>>,
}

impl FileReader {
    pub fn new() -> Rc<Self> {
        Self {
            opened_files: HashMap::new().into(),
        }
        .into()
    }

    /// If the file is cached, return the cached string. Otherwise read it fresh from the file
    /// system and cache it.
    /// Returns `None` if `path` is invalid.
    pub fn read_file(&self, path: &'static str) -> Option<&'static SourceString> {
        let mut opened_files = self.opened_files.borrow_mut();
        if let Some(&s) = opened_files.get(path) {
            return Some(s);
        }
        let file_content: &'static SourceString = {
            let source = fs::read_to_string(path).ok()?;
            SourceString::new(source.leak())
        };
        opened_files.insert(path, file_content);
        Some(file_content)
    }

    /// Used in testing when there is no actual source file.
    /// Panics if inserting a `source` onto the same `path` multiple times.
    /// Overwriting a `source` onto an ocupied `path` is considered undefined behavior since during
    /// error report the program would try to index the string with `SourceIdx` under the
    /// assumption that the `SourceString` obtained by the same `path` would be the same.
    #[allow(dead_code)]
    pub fn manual_insert(&self, path: &'static str, source: &'static SourceString) {
        let mut map = self.opened_files.borrow_mut();
        let was_empty = map.insert(path, source).is_none();
        assert!(was_empty)
    }
}
