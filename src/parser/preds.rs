//! The predicate functions used by the parser to make it easy to consume input.

use crate::tokenizer::Token;

pub fn mergepreds<'s, T>(
    a: impl Fn(&Token<'s>) -> Option<T>,
    b: impl Fn(&Token<'s>) -> Option<T>,
) -> impl Fn(&Token<'s>) -> Option<T> {
    move |t| {
        if let Some(v) = a(t) {
            Some(v)
        } else if let Some(v) = b(t) {
            Some(v)
        } else {
            None
        }
    }
}

pub fn to_bpred<'s, T>(
    pred: impl Fn(&Token<'s>) -> Option<T>,
) -> impl Fn(&Token<'s>) -> Option<()> {
    move |t| match pred(t) {
        Some(_) => Some(()),
        None => None,
    }
}

macro_rules! bpred {
    ($($($pattern:pat_param)|+ $(if $guard:expr)?),* $(,)?) => {
        |t: &Token<'s>| match t.kind {
            $($($pattern)|+ $(if $guard)? => Some(()),)*
            _ => None,
        }
    };
}

macro_rules! tpred {
    ($($($pattern:pat_param)|+ $(if $guard:expr)?),* $(,)?) => {
        |t| match t.kind {
            $($($pattern)|+ $(if $guard)? => Some(t.clone()),)*
            _ => None,
        }
    };
}

macro_rules! vpred {
    ($($(:$t:ident:)? $($pattern:pat_param)|+ $(if $guard:expr)? => $val:expr),* $(,)?) => {
        |t| match t.kind {
            $($($pattern)|+ $(if $guard)? => {$(let $t = t;)? Some($val)})*
            _ => None,
        }
    };
}

pub(super) use {bpred, tpred, vpred};
