//! This stage resolves references to symbols across a compilation unit.
//! This does not include object properties - that happens during typechecking.
//!
//! Since each compilation unit can contain multiple files, and we don't know
//! which files are included until they are parsed, this stage is what actually
//! calls the lexer and parser.
//! 
use std::path::Path;

mod ast;
pub use ast::*;

use crate::{parser, string_storage::StringStorage};

pub fn resolve<'a>(
    storage: &'a StringStorage,
    parse: impl Fn(&Path) -> parser::Expr<'a>,
) -> Expr<'a> {
    todo!()
}

struct Resolver {

}
