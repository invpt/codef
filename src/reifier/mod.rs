//! Resolves references, translates the AST to more of a semantics tree.

use std::{path::Path, collections::HashMap};

mod rst;
pub use rst::*;
use rustc_hash::FxHashMap;

use crate::{parser, string_storage::StringStorage, tokenizer::Span};

#[derive(Debug)]
pub struct ReifyError<'s> {
    pub kind: ReifyErrorKind<'s>,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum ReifyErrorKind<'s> {
    InvalidFile,
    UnexpectedTopLevelStatement,
    X(&'s i32),
}

type Result<'s, T> = std::result::Result<T, ReifyError<'s>>;

pub fn resolve<'s>(
    storage: &'s StringStorage,
    expr: parser::Expr<'s>,
) -> Result<'s, Module<'s>> {
    Resolver { module: Module { defs: FxHashMap::default() }}.resolve(expr)
}

struct Resolver<'s> {
    module: Module<'s>,
}

impl<'s> Resolver<'s> {
    fn resolve(mut self, expr: parser::Expr<'s>) -> Result<'s, Module<'s>> {
        // The top-level should be a scope
        let parser::Expr {
            kind: parser::ExprKind::Scope(parser::Scope { defs, exprs, .. }),
            ..
        } = expr else {
            return Err(ReifyError {
                kind: ReifyErrorKind::InvalidFile,
                span: None,
            })
        };
        // There cannot be any statements at the top-level
        if let [expr, ..] = &*exprs {
            return Err(ReifyError {
                kind: ReifyErrorKind::UnexpectedTopLevelStatement,
                span: Some(expr.span),
            })
        }

        Ok(self.module)
    }
}
