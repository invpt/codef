use std::num::NonZeroUsize;

use rustc_hash::FxHashMap;

use crate::tokenizer::{Intern, Span};

pub use crate::parser::{BinOp, UnOp, Literal, SolveMarker};

#[derive(Debug, Default)]
pub struct Module<'s> {
    pub main: Option<Symbol>,
    pub defs: FxHashMap<Symbol, Proc<'s>>,
    pub types: FxHashMap<Symbol, TypeDef<'s>>,
    pub locals: FxHashMap<Symbol, Local<'s>>,
}

#[derive(Debug)]
pub struct TypeDef<'s> {
    pub name: Intern<'s>,
    pub inner: Type<'s>,
}

#[derive(Debug)]
pub struct Type<'s> {
    pub kind: TypeKind<'s>,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum TypeKind<'s> {
    Abstract(Option<Box<Type<'s>>>, Box<Type<'s>>),
    Variant(Box<[VariantItemType<'s>]>),
    Tuple(Box<[Type<'s>]>),
    Symbol(Symbol),
}

#[derive(Debug)]
pub struct VariantItemType<'s> {
    name: Intern<'s>,
    inner: Type<'s>,
}

#[derive(Debug)]
pub struct Local<'s> {
    name: Intern<'s>,
    mutable: bool,
    ty: Option<Expr<'s>>
}

#[derive(Debug)]
pub struct Proc<'s> {
    pub name: Intern<'s>,
    pub spec: bool,
    pub arg: Option<Pattern<'s>>,
    pub body: Expr<'s>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Symbol(pub(super) NonZeroUsize);


#[derive(Debug)]
pub struct Scope<'s> {
    pub exprs: Box<[Expr<'s>]>,
    pub discard: bool,
}

#[derive(Debug)]
pub struct Expr<'s> {
    pub kind: ExprKind<'s>,
    pub span: Span,
    pub ty: Option<Type<'s>>,
}

#[derive(Debug)]
pub enum ExprKind<'s> {
    Scope(Scope<'s>),
    Abstract {
        spec: bool,
        arg: Option<Pattern<'s>>,
        body: Box<Expr<'s>>,
    },
    For {
        init: Option<Box<Expr<'s>>>,
        cond: Box<Expr<'s>>,
        afterthought: Option<Box<Expr<'s>>>,
        body: Box<Expr<'s>>,
    },
    Case {
        cond: Box<Expr<'s>>,
        on_true: Box<Expr<'s>>,
        on_false: Option<Box<Expr<'s>>>,
    },
    Tuple {
        items: Box<[Expr<'s>]>,
    },
    StructuralEq(Box<Pattern<'s>>, Box<Expr<'s>>),
    Binary(BinOp, Box<Expr<'s>>, Box<Expr<'s>>),
    Unary(UnOp, Box<Expr<'s>>),
    Apply(Box<Expr<'s>>, Box<Expr<'s>>),
    Literal(Literal<'s>),
    Symbol(Symbol),
}

#[derive(Debug)]
pub struct Pattern<'s> {
    kind: PatternKind<'s>,
    span: Span,
}

#[derive(Debug)]
pub enum PatternKind<'s> {
    Variant(Intern<'s>, Box<Pattern<'s>>),
    Tuple(Box<[Pattern<'s>]>),
    Solve(SolveMarker, Symbol),
}
