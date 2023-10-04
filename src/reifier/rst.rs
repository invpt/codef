use std::{hash::Hash, num::NonZeroUsize};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{string_storage::Intern, tokenizer::Span};

pub use crate::parser::{BinOp, Literal, SolveMarker, UnOp};

#[derive(Debug, Default)]
pub struct Module<'s> {
    pub main: Option<Symbol>,
    pub defs: FxHashMap<Symbol, Def<'s>>,
    pub typedefs: FxHashMap<Symbol, TypeDef<'s>>,
    pub locals: FxHashMap<Symbol, Local<'s>>,
    pub builtin_funcs: FxHashMap<Symbol, (Option<Type<'s>>, Type<'s>)>,
}

#[derive(Debug)]
pub struct TypeDef<'s> {
    pub decl_span: Span,
    pub name: Intern<'s>,
    pub inner: Type<'s>,
}

impl<'s> Type<'s> {
    pub fn is_int(&self) -> bool {
        self.is_subtype(&(Type::Primitive(PrimitiveType::Integer)))
    }

    pub fn is_float(&self) -> bool {
        self.is_subtype(&(Type::Primitive(PrimitiveType::Float)))
    }

    pub fn is_bool(&self) -> bool {
        self.is_subtype(&(Type::Primitive(PrimitiveType::Boolean)))
    }
}

impl<'s> Type<'s> {
    /// Tries to widen with the other type.
    pub fn widen(&self, other: &Type<'s>) -> Option<Type<'s>> {
        if let (Type::Variant(a), Type::Variant(b)) = (self, other) {
            // a, b must be nonoverlapping in names.
            // if so, we can just concat their variants
            let mut items = Vec::with_capacity(a.len() + b.len());
            for a in a.iter() {
                let mut added = false;
                for b in b.iter() {
                    if a.name == b.name {
                        // then their values must be able to widen too...
                        if let (Some(ain), Some(bin)) = (&a.inner, &b.inner) {
                            if let Some(win) = ain.widen(bin) {
                                items.push(VariantItemType {
                                    name: a.name,
                                    inner: Some(win),
                                });
                                added = true;
                            } else {
                                return None;
                            }
                        } else {
                            return None;
                        }
                    } else {
                        items.push(b.clone())
                    }
                }

                if !added {
                    items.push(a.clone())
                }
            }

            None
        } else if self.is_subtype(other) {
            Some(other.clone())
        } else if other.is_subtype(self) {
            Some(self.clone())
        } else {
            None
        }
    }

    pub fn is_subtype(&self, other: &Type<'s>) -> bool {
        match (self, other) {
            (Type::Function(aarg, aret), Type::Function(barg, bret)) => {
                if let (Some(aarg), Some(barg)) = (aarg, barg) {
                    barg.is_subtype(aarg) && aret.is_subtype(bret)
                } else if let (None, None) = (aarg, barg) {
                    aret.is_subtype(bret)
                } else {
                    false
                }
            }
            (Type::Variant(a), Type::Variant(b)) => {
                for a in a.iter() {
                    let mut found = false;
                    for b in b.iter() {
                        if a.name != b.name {
                            continue;
                        }
                        if let (Some(ai), Some(bi)) = (&a.inner, &b.inner) {
                            found = ai.is_subtype(bi)
                        } else if let (None, None) = (&a.inner, &b.inner) {
                            found = true
                        }
                    }
                    if !found {
                        return false;
                    }
                }

                true
            }
            (Type::Tuple(a), Type::Tuple(b)) => {
                if a.len() != b.len() {
                    return false;
                }

                for (a, b) in a.iter().zip(b.iter()) {
                    if !a.is_subtype(b) {
                        return false;
                    }
                }

                true
            }
            (Type::Constructor(a), Type::Constructor(b)) => a == b,
            (Type::Symbol(a), Type::Symbol(b)) => a == b,
            (Type::Primitive(a), Type::Primitive(b)) => a == b,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type<'s> {
    Function(Option<Box<Type<'s>>>, Box<Type<'s>>),
    Variant(Box<[VariantItemType<'s>]>),
    Tuple(Box<[Type<'s>]>),
    Constructor(Symbol),
    Symbol(Symbol),
    Primitive(PrimitiveType),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    Float,
    Integer,
    String,
    Boolean,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariantItemType<'s> {
    pub name: Intern<'s>,
    pub inner: Option<Type<'s>>,
}

#[derive(Debug)]
pub struct Local<'s> {
    pub decl_span: Span,
    pub name: Intern<'s>,
    pub mutable: bool,
    pub ty: Type<'s>,
}

#[derive(Debug)]
pub struct Def<'s> {
    pub decl_span: Span,
    pub name: Intern<'s>,
    pub spec: bool,
    pub arg: Option<Pattern<'s>>,
    pub body: Expr<'s>,
    pub ty: Type<'s>,
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
    pub ty: Type<'s>,
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
    Tuple(Box<[Expr<'s>]>),
    StructuralEq(Box<Pattern<'s>>, Box<Expr<'s>>),
    Binary(BinOp, Box<Expr<'s>>, Box<Expr<'s>>),
    Unary(UnOp, Box<Expr<'s>>),
    Call(Box<Expr<'s>>, Box<Expr<'s>>),
    Construct(Symbol, Box<Expr<'s>>),
    Variant(Intern<'s>, Option<Box<Expr<'s>>>),
    Symbol(Symbol),
    Literal(Literal<'s>),
}

#[derive(Debug)]
pub struct Pattern<'s> {
    pub kind: PatternKind<'s>,
    pub span: Span,
    pub ty: Type<'s>,
}

#[derive(Debug)]
pub enum PatternKind<'s> {
    Apply(Box<Pattern<'s>>, Box<Pattern<'s>>),
    Variant(Intern<'s>, Option<Box<Pattern<'s>>>),
    Tuple(Box<[Pattern<'s>]>),
    Solve(SolveMarker, Symbol),
    Symbol(Symbol),
}

#[derive(Debug)]
pub struct VariantItem<'s> {
    pub name: Intern<'s>,
    pub value: Option<Box<Expr<'s>>>,
    pub span: Span,
}
