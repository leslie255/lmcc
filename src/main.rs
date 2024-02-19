#![feature(decl_macro)]

use std::{mem::MaybeUninit, sync::Mutex};

use ast::Expr;
use error::{Error, ToSpanned};
use intern_str::{InternStr, MaybeOwnedStr, StringArena};
use mir::make_mir_for_item;
use parser::Parser;

use crate::{error::ErrorReporter, file_reader::FileReader, mir::NamesContext, token::TokenStream};

mod ast;
mod error;
mod file_reader;
mod intern_str;
mod mir;
mod parser;
mod source_string;
mod token;
mod utils;

static mut GLOBAL_STR_CX: MaybeUninit<Mutex<StringArena<'static>>> = MaybeUninit::uninit();

fn init_global_str_cx() {
    unsafe { GLOBAL_STR_CX = MaybeUninit::new(Mutex::new(StringArena::new())) };
}

#[cfg_attr(debug_assertions, track_caller)]
pub fn intern_string(s: impl Into<MaybeOwnedStr<'static>> + AsRef<str>) -> InternStr<'static> {
    let global_str_cx = if cfg!(debug_assertions) {
        unsafe { GLOBAL_STR_CX.assume_init_ref().lock().unwrap() }
    } else {
        unsafe { GLOBAL_STR_CX.assume_init_ref().lock().unwrap_unchecked() }
    };
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
        args.next().expect("Expect one argument for input file")
    }
    .leak();
    let source = file_reader.read_file(path).unwrap();
    let token_stream = TokenStream::new(path, source, err_reporter.clone(), file_reader.clone());
    let ast_parser = Parser::new(err_reporter.clone(), token_stream.peekable());

    err_reporter.exit_if_has_error();

    let mut global_cx = NamesContext::default();
    for expr in ast_parser {
        let (expr, span) = expr.into_pair();
        dbg!(&expr);
        err_reporter.exit_if_has_error();
        match expr {
            Expr::Decl(item) => make_mir_for_item(&mut global_cx, item, err_reporter.clone()),
            Expr::DeclList(items) => {
                for item in items {
                    make_mir_for_item(&mut global_cx, item, err_reporter.clone());
                }
            }
            Expr::EmptyDecl(_) => todo!(),
            _ => {
                err_reporter.report(&Error::ExprNotAllowed.to_spanned(span));
            }
        }
    }

    dbg!(global_cx);

    err_reporter.exit_if_has_error();
}
