use std::{marker::PhantomData, io};

use crate::{parser::ParseErrorKind, tokenizer::{TokenizationError, TokenizationErrorKind, Span}};

pub struct ErrorStream<'s> {
    phantom: PhantomData<&'s str>,
}

impl<'s> ErrorStream<'s> {
    pub fn new() -> ErrorStream<'s> {
        ErrorStream { phantom: PhantomData }
    }

    pub fn warning(&self, warning: impl Into<CompilationError<'s>>) {
        eprintln!("WARNING: {:?}", warning.into())
    }

    pub fn error(&self, error: impl Into<CompilationError<'s>>) {
        eprintln!("ERROR: {:?}", error.into())
    }
}

#[derive(Debug)]
pub struct CompilationError<'s> {
    pub kind: CompilationErrorKind<'s>,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum CompilationErrorKind<'s> {
    Parse(ParseErrorKind<'s>),
    Tokenization(TokenizationErrorKind),
    Io(io::Error),
}

impl<'s> From<ParseErrorKind<'s>> for CompilationError<'s> {
    fn from(err: ParseErrorKind<'s>) -> Self {
        if let ParseErrorKind::TokenizationError(err) = err {
            err.into()
        } else {
            CompilationError {
                kind: CompilationErrorKind::Parse(err),
                span: None,
            }
        }
    }
}

impl<'s> From<TokenizationError> for CompilationError<'s> {
    fn from(err: TokenizationError) -> Self {
        if let TokenizationErrorKind::Io(io_err) = err.kind {
            (io_err, err.span).into()
        } else {
            CompilationError {
                kind: CompilationErrorKind::Tokenization(err.kind),
                span: err.span,
            }
        }
    }
}

impl<'s> From<(io::Error, Option<Span>)> for CompilationError<'s> {
    fn from((err, span): (io::Error, Option<Span>)) -> Self {
        CompilationError {
            kind: CompilationErrorKind::Io(err),
            span,
        }
    }
}