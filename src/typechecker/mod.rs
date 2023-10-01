use std::marker::PhantomData;

use rustc_hash::FxHashMap;

use crate::{reifier::*, string_storage::Intern, tokenizer::Span};

#[derive(Debug)]
pub struct TypeError<'s> {
    pub kind: TypeErrorKind<'s>,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum TypeErrorKind<'s> {
    NotValidHere,
    CannotBeInferred,
    MissingReturnType,
    InvalidApplication,
    Mismatch,
    Something(Intern<'s>),
}

type Result<'s, T> = std::result::Result<T, TypeError<'s>>;

pub fn typecheck<'s>(module: &mut Module<'s>) -> Result<'s, ()> {
    let mut checker = Typechecker {
        typedefs: std::mem::take(&mut module.typedefs),
        builtin_funcs: std::mem::take(&mut module.builtin_funcs),
        ..Default::default()
    };
    checker.typeck(module)?;
    module.typedefs = checker.typedefs;
    module.builtin_funcs = checker.builtin_funcs;
    Ok(())
}

#[derive(Debug, Default)]
struct Typechecker<'s> {
    typedefs: FxHashMap<Symbol, TypeDef<'s>>,
    builtin_funcs: FxHashMap<Symbol, (Option<Type<'s>>, Type<'s>)>,
    local_types: FxHashMap<Symbol, Type<'s>>,
    def_types: FxHashMap<Symbol, Type<'s>>,
}

impl<'s> Typechecker<'s> {
    fn typeck(&mut self, module: &mut Module<'s>) -> Result<'s, ()> {
        for (sym, def) in &mut module.defs {
            if let Some(ret_ty) = &def.body.ty {
                let arg_ty = if let Some(arg) = &mut def.arg {
                    Some(self.pattern(None, arg)?.clone())
                } else {
                    None
                };
                self.def_types.insert(
                    *sym,
                    Type::inferred(TypeKind::Function(
                        arg_ty.map(Box::new),
                        Box::new(ret_ty.clone()),
                    )),
                );
            }
        }

        // TODO: traverse in topological order
        for (sym, def) in &mut module.defs {
            if !self.def_types.contains_key(sym) {
                let arg_ty = if let Some(arg) = &mut def.arg {
                    Some(self.pattern(None, arg)?.clone())
                } else {
                    None
                };
                let ret_ty = self.expr(&mut def.body)?;

                self.def_types.insert(
                    *sym,
                    Type {
                        kind: TypeKind::Function(arg_ty.map(Box::new), Box::new(ret_ty.clone())),
                        span: None,
                    },
                );
            } else {
                self.expr(&mut def.body)?;
            }
        }

        Ok(())
    }

    // Bottom-up
    fn expr<'a>(&mut self, expr: &'a mut Expr<'s>) -> Result<'s, &'a Type<'s>> {
        let kind = match &mut expr.kind {
            ExprKind::Scope(scope) => {
                let discard = scope.discard;
                let len = scope.exprs.len();
                let mut final_ty = None;
                for (i, expr) in scope.exprs.iter_mut().enumerate() {
                    let ty = self.expr(expr)?;
                    if i == len - 1 && !discard {
                        final_ty = Some(ty.kind.clone())
                    }
                }

                if discard || len == 0 {
                    TypeKind::Tuple(Box::new([]))
                } else {
                    final_ty.unwrap()
                }
            }
            ExprKind::Abstract { arg, body, .. } => {
                let arg_type = if let Some(arg) = arg {
                    Some(self.pattern(None, arg)?.clone())
                } else {
                    None
                };
                let body_type = self.expr(body)?;

                TypeKind::Function(arg_type.map(Box::new), Box::new(body_type.clone()))
            }
            ExprKind::For {
                init,
                cond,
                afterthought,
                body,
            } => {
                if let Some(init) = init {
                    self.expr(init)?;
                }
                self.expr(cond)?;
                if let Some(afterthought) = afterthought {
                    self.expr(afterthought)?;
                }

                let expected_type = Type::inferred(TypeKind::Tuple(Box::new([])));
                let body_type = self.expr(body)?;
                if !body_type.is_subtype(&expected_type) {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }

                expected_type.kind
            }
            ExprKind::Case {
                cond,
                on_true,
                on_false,
            } => {
                let cond_type = self.expr(cond)?;
                if !cond_type
                    .is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Boolean)))
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }
                let on_true_type = self.expr(on_true)?.clone();

                if let Some(on_false) = on_false {
                    let on_false_type = self.expr(on_false)?;

                    let Some(both) = on_true_type.widen(on_false_type) else {
                        return Err(TypeError {
                            kind: TypeErrorKind::InvalidApplication,
                            span: Some(expr.span),
                        })
                    };

                    both.kind
                } else if on_true_type.is_subtype(&Type::inferred(TypeKind::Tuple(Box::new([])))) {
                    TypeKind::Tuple(Box::new([]))
                } else {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    })
                }
            }
            ExprKind::Tuple { items } => {
                let mut item_types = Vec::with_capacity(items.len());
                for item in items.iter_mut() {
                    item_types.push(self.expr(item)?.clone());
                }
                TypeKind::Tuple(item_types.into_boxed_slice())
            }
            ExprKind::StructuralEq(a, b) => {
                let b_type = self.expr(b)?;
                self.pattern(Some(b_type), a)?;

                TypeKind::Primitive(PrimitiveType::Boolean)
            }
            ExprKind::Binary(BinOp::Or | BinOp::And, a, b) => {
                if !self
                    .expr(a)?
                    .is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Boolean)))
                    || !self
                        .expr(b)?
                        .is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Boolean)))
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }

                TypeKind::Primitive(PrimitiveType::Boolean)
            }
            ExprKind::Binary(BinOp::Eq | BinOp::Neq, a, b) => {
                let a_type = self.expr(a)?.clone();
                let b_type = self.expr(b)?;
                if a_type.widen(b_type).is_none() {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }

                TypeKind::Primitive(PrimitiveType::Boolean)
            }
            ExprKind::Binary(BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq, a, b) => {
                let a_type = self.expr(a)?.clone();
                if !a_type.is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Integer)))
                    && !a_type
                        .is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Float)))
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }
                let b_type = self.expr(b)?;
                if !b_type.is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Integer)))
                    && !b_type
                        .is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Float)))
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }
                if !a_type.is_subtype(b_type) {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }

                TypeKind::Primitive(PrimitiveType::Boolean)
            }
            ExprKind::Binary(
                BinOp::BitOr | BinOp::BitXor | BinOp::BitAnd | BinOp::Shl | BinOp::Shr,
                a,
                b,
            ) => {
                let a_type = self.expr(a)?.clone();
                if !a_type.is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Integer)))
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }
                let b_type = self.expr(b)?;
                if !b_type.is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Integer)))
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }
                if !a_type.is_subtype(b_type) {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }

                b_type.kind.clone()
            }
            ExprKind::Binary(
                BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div | BinOp::Mod,
                a,
                b,
            ) => {
                let a_type = self.expr(a)?.clone();
                if !a_type.is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Integer)))
                    && !a_type
                        .is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Float)))
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }
                let b_type = self.expr(b)?;
                if !b_type.is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Integer)))
                    && !b_type
                        .is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Float)))
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }
                if !a_type.is_subtype(b_type) {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }

                b_type.kind.clone()
            }
            ExprKind::Binary(_, _, _) => todo!(),
            ExprKind::Unary(UnOp::Neg, a) => {
                let a_type = self.expr(a)?;
                if !a_type.is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Integer)))
                    && !a_type
                        .is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Float)))
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }

                a_type.kind.clone()
            }
            ExprKind::Unary(UnOp::Not, a) => {
                if !self
                    .expr(a)?
                    .is_subtype(&Type::inferred(TypeKind::Primitive(PrimitiveType::Boolean)))
                {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    });
                }

                TypeKind::Primitive(PrimitiveType::Boolean)
            }
            ExprKind::Apply(base, arg) => match self.expr(base)?.kind.clone() {
                TypeKind::Function(param, ret) => {
                    let arg_type = self.expr(arg)?;
                    let Some(param) = param else {
                        return Err(TypeError {
                            kind: TypeErrorKind::InvalidApplication,
                            span: Some(expr.span),
                        })
                    };

                    if arg_type.is_subtype(&param) {
                        ret.kind.clone()
                    } else {
                        return Err(TypeError {
                            kind: TypeErrorKind::InvalidApplication,
                            span: Some(expr.span),
                        });
                    }
                }
                TypeKind::Constructor(sym) => {
                    // check validity of arg
                    let arg_type = self.expr(arg)?.clone();
                    let typedef = self.typedefs.get(&sym).unwrap();
                    if arg_type.is_subtype(&typedef.inner) {
                        TypeKind::Symbol(sym)
                    } else {
                        return Err(TypeError {
                            kind: TypeErrorKind::InvalidApplication,
                            span: Some(expr.span),
                        });
                    }
                }
                _ => {
                    return Err(TypeError {
                        kind: TypeErrorKind::InvalidApplication,
                        span: Some(expr.span),
                    })
                }
            },
            ExprKind::Variant(name, value) => TypeKind::Variant(Box::new([VariantItemType {
                name: *name,
                inner: if let Some(value) = value {
                    Some(self.expr(value)?.clone())
                } else {
                    None
                },
            }])),
            ExprKind::Symbol(sym) => {
                if let Some(local_type) = self.local_types.get(sym) {
                    local_type.kind.clone()
                } else if let Some(def_type) = self.def_types.get(sym) {
                    def_type.kind.clone()
                } else if let Some(_) = self.typedefs.get(sym) {
                    TypeKind::Constructor(*sym)
                } else if let Some((arg, ret)) = self.builtin_funcs.get(sym) {
                    TypeKind::Function(arg.clone().map(Box::new), Box::new(ret.clone()))
                } else {
                    return Err(TypeError {
                        kind: TypeErrorKind::NotValidHere,
                        span: Some(expr.span),
                    });
                }
            }
            ExprKind::Literal(Literal::Float(_)) => TypeKind::Primitive(PrimitiveType::Float),
            ExprKind::Literal(Literal::Integer(_)) => TypeKind::Primitive(PrimitiveType::Integer),
            ExprKind::Literal(Literal::String(_)) => TypeKind::Primitive(PrimitiveType::String),
            ExprKind::Literal(Literal::Boolean(_)) => TypeKind::Primitive(PrimitiveType::Boolean),
        };

        let inferred = Type::inferred(kind);
        if let Some(ref existing_ty) = expr.ty {
            if inferred.is_subtype(existing_ty) {
                Ok(existing_ty)
            } else {
                Err(TypeError {
                    kind: TypeErrorKind::Mismatch,
                    span: Some(expr.span),
                })
            }
        } else {
            Ok(expr.ty.insert(inferred))
        }
    }

    // Top-down
    fn pattern<'a>(
        &mut self,
        ty: Option<&'a Type<'s>>,
        pat: &'a mut Pattern<'s>,
    ) -> Result<'s, &'a Type<'s>> {
        // weaken lifetime
        let mut ty: Option<&Type<'s>> = ty;
        let mut found = None;
        if let Some(patty) = &pat.ty {
            if let Some(ty) = ty {
                if !ty.is_subtype(patty) {
                    return Err(TypeError {
                        kind: TypeErrorKind::Mismatch,
                        span: Some(pat.span),
                    });
                }
            } else {
                found = pat.ty.take();
                ty = found.as_ref();
            }
        };

        let inferred = match &mut pat.kind {
            PatternKind::Apply(base, arg) => {
                let sym = if let Some(ty) = ty {
                    let TypeKind::Symbol(sym) = &ty.kind else {
                        return Err(TypeError {
                            kind: TypeErrorKind::Mismatch,
                            span: Some(pat.span),
                        })
                    };

                    self.pattern(
                        Some(&Type {
                            kind: TypeKind::Constructor(*sym),
                            span: None,
                        }),
                        base,
                    )?;

                    *sym
                } else {
                    let TypeKind::Constructor(sym) = self.pattern(None, base)?.kind else {
                        return Err(TypeError {
                            kind: TypeErrorKind::Mismatch,
                            span: Some(pat.span),
                        })
                    };
                    sym
                };

                let typedef = self.typedefs.get(&sym).unwrap();
                self.pattern(Some(&typedef.inner.clone()), arg)?;
                TypeKind::Symbol(sym)
            }
            PatternKind::Variant(name, value) => {
                if let Some(ty) = ty {
                    let TypeKind::Variant(items) = &ty.kind else {
                        return Err(TypeError {
                            kind: TypeErrorKind::Mismatch,
                            span: Some(pat.span),
                        })
                    };

                    let Some(variant) = items.iter().filter(|it| it.name == *name).next() else {
                        return Err(TypeError {
                            kind: TypeErrorKind::Mismatch,
                            span: Some(pat.span),
                        })
                    };

                    match (value, &variant.inner) {
                        (Some(value), Some(inner)) => {
                            self.pattern(Some(inner), value)?;
                        }
                        (Some(_), None) | (None, Some(_)) => {
                            return Err(TypeError {
                                kind: TypeErrorKind::Mismatch,
                                span: Some(pat.span),
                            });
                        }
                        (None, None) => {}
                    }

                    ty.kind.clone()
                } else {
                    TypeKind::Variant(Box::new([VariantItemType {
                        name: *name,
                        inner: if let Some(value) = value {
                            Some(self.pattern(None, value)?.clone())
                        } else {
                            None
                        },
                    }]))
                }
            }
            PatternKind::Tuple(items) => {
                if let Some(ty) = ty {
                    let TypeKind::Tuple(ty_items) = &ty.kind else {
                        return Err(TypeError {
                            kind: TypeErrorKind::Mismatch,
                            span: Some(pat.span),
                        })
                    };

                    if items.len() != ty_items.len() {
                        return Err(TypeError {
                            kind: TypeErrorKind::Mismatch,
                            span: Some(pat.span),
                        });
                    }

                    for (it, ty_it) in items.iter_mut().zip(ty_items.iter()) {
                        self.pattern(Some(ty_it), it)?;
                    }

                    ty.kind.clone()
                } else {
                    let mut item_tys = Vec::with_capacity(items.len());
                    for it in items.iter_mut() {
                        item_tys.push(self.pattern(None, it)?.clone());
                    }
                    TypeKind::Tuple(item_tys.into_boxed_slice())
                }
            }
            PatternKind::Solve(SolveMarker::Val | SolveMarker::Var, sym) => {
                if let Some(ty) = ty {
                    self.local_types.insert(*sym, ty.clone());
                    ty.kind.clone()
                } else {
                    return Err(TypeError {
                        kind: TypeErrorKind::CannotBeInferred,
                        span: Some(pat.span),
                    });
                }
            }
            PatternKind::Solve(SolveMarker::Set, sym) => {
                let local_ty = self.local_types.get(&sym).unwrap();

                /*if !ty.is_subtype(local_ty) {
                    return Err(TypeError {
                        kind: TypeErrorKind::Mismatch,
                        span: Some(pat.span),
                    });
                }*/
                local_ty.kind.clone()
            }
            PatternKind::Symbol(sym) => {
                /*if !ty.is_subtype(&Type::inferred(TypeKind::Symbol(*sym))) {
                    return Err(TypeError {
                        kind: TypeErrorKind::Mismatch,
                        span: Some(pat.span),
                    });
                }*/
                TypeKind::Constructor(*sym)
            }
        };

        let inferred = Type::inferred(inferred);
        if let Some(ty) = ty {
            if inferred.is_subtype(ty) {
                Ok(pat.ty.insert(inferred))
            } else {
                Err(TypeError {
                    kind: TypeErrorKind::Mismatch,
                    span: Some(pat.span),
                })
            }
        } else {
            Ok(pat.ty.insert(inferred))
        }

        /*if let Some(ref existing_ty) = pat.ty {
            if ty.is_subtype(existing_ty) {
                Ok(())
            } else {
                Err(TypeError {
                    kind: TypeErrorKind::Mismatch,
                    span: Some(pat.span),
                })
            }
        } else {
            pat.ty = Some(ty.clone());
            Ok(())
        }*/
    }
}
