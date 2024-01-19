use std::{hash::Hash, num::NonZeroUsize};

use rustc_hash::FxHashMap;

use crate::{string_storage::Intern, tokenizer::Span};

pub use crate::parser::{BinOp, Literal, SolveMarker, UnOp};

#[derive(Debug, Default)]
pub struct Module<'s> {
    pub main: Option<Symbol>,
    pub defs: FxHashMap<Symbol, Def<'s>>,
    pub locals: FxHashMap<Symbol, Local<'s>>,
    pub builtins: FxHashMap<Symbol, (Builtin, Type<'s>)>,
}

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Alloc,
    Spec,
    Print,
    Println,
    Input,
    Itoa,
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

    pub fn is_unit(&self) -> bool {
        self.is_subtype(&Type::Tuple(Box::new([])))
    }

    pub fn is_unknown(&self) -> bool {
        match self {
            Type::Unknown => true,
            _ => false,
        }
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
                // self is a subtype of other if it only contains variants that are subtypes of variants in other
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
            (Type::Instance(a), Type::Instance(b)) => a == b,
            (Type::Primitive(a), Type::Primitive(b)) => a == b,
            (_, Type::Unknown) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type<'s> {
    Function(Option<Box<Type<'s>>>, Box<Type<'s>>),
    Variant(Box<[VariantItemType<'s>]>),
    Tuple(Box<[Type<'s>]>),
    Instance(Symbol),
    Primitive(PrimitiveType),
    Unknown,
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
    pub body: Expr<'s>,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Symbol(pub(super) NonZeroUsize);

impl Symbol {
    pub fn index(self) -> usize {
        self.0.into()
    }
}

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
    Apply(Box<Expr<'s>>, Box<Expr<'s>>),
    Variant(Intern<'s>, Option<Box<Expr<'s>>>),
    Constructor(Symbol),
    Load(Symbol),
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
