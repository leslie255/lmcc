#![feature(decl_macro, str_internals, result_option_inspect)]

use std::{mem::MaybeUninit, sync::Mutex};

use ast::Parser;
use intern_str::{InternStr, MaybeOwnedStr, StringArena};

use crate::{error::ErrorReporter, file_reader::FileReader, token::TokenStream};

mod ast;
mod error;
mod file_reader;
mod intern_str;
mod source_string;
mod token;
mod utils;

static mut GLOBAL_STR_CX: MaybeUninit<Mutex<StringArena<'static>>> = MaybeUninit::uninit();
fn init_global_str_cx() {
    unsafe { GLOBAL_STR_CX = MaybeUninit::new(Mutex::new(StringArena::new())) };
}
#[track_caller] // for mutex lock error
pub fn intern_string(s: impl Into<MaybeOwnedStr<'static>> + AsRef<str>) -> InternStr<'static> {
    let global_str_cx = unsafe { GLOBAL_STR_CX.assume_init_ref() }.lock().unwrap();
    let interned = global_str_cx.intern(s);
    unsafe { std::mem::transmute(interned) }
}
fn main() {
    init_global_str_cx();
    let file_reader = FileReader::new();
    let err_reporter = ErrorReporter::new(file_reader.clone());
    let path = {
        let mut args = std::env::args();
        args.next().unwrap();
        args.next().unwrap()
    }.leak();
    let source = file_reader.read_file(path).unwrap();
    let token_stream = TokenStream::new(path, source, err_reporter.clone(), file_reader.clone());
    let ast_parser = Parser::new(err_reporter.clone(), token_stream.peekable());
    for expr in ast_parser {
        dbg!(expr);
    }
    err_reporter.exit_if_has_error();
}
