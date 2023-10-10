//! Resolves references, translates the AST to more of a semantics tree.

use std::ops::Deref;

mod rst;
mod scoper;
pub use rst::*;
use rustc_hash::FxHashMap;

use crate::{
    parser,
    string_storage::{Intern, StringInterner},
    tokenizer::Span,
};

use self::scoper::Scoper;

#[derive(Debug)]
pub struct ReifyError<'s> {
    pub kind: ReifyErrorKind<'s>,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum ReifyErrorKind<'s> {
    InvalidFile,
    UnexpectedTopLevelStatement,
    UndefinedSymbol(Intern<'s>),
    UnexpectedMarker(SolveMarker),
    ImpossibleSolve,
    InvalidType,
    InvalidVariant,
    InvalidPattern,
    TypeAssertionConflict,
}

type Result<'s, T> = std::result::Result<T, ReifyError<'s>>;

pub fn reify<'s>(
    interner: &'s mut StringInterner<'s>,
    expr: &parser::Expr<'s>,
) -> Result<'s, Module<'s>> {
    Reifier {
        interner,
        scoper: Scoper::default(),
        module: Module::default(),
        def_types: FxHashMap::default(),
        builtin_types: FxHashMap::default(),
    }
    .reify(expr)
}

struct Reifier<'s> {
    interner: &'s mut StringInterner<'s>,
    scoper: Scoper<'s>,
    module: Module<'s>,
    def_types: FxHashMap<Symbol, Type<'s>>,
    builtin_types: FxHashMap<Symbol, Type<'s>>,
}

impl<'s> Reifier<'s> {
    fn reify(mut self, expr: &parser::Expr<'s>) -> Result<'s, Module<'s>> {
        // The top-level should be a scope
        let parser::Expr {
            kind: parser::ExprKind::Scope(scope),
            ..
        } = expr else {
            return Err(ReifyError {
                kind: ReifyErrorKind::InvalidFile,
                span: None,
            })
        };
        // There cannot be any statements at the top-level
        if let [expr, ..] = &*scope.exprs {
            return Err(ReifyError {
                kind: ReifyErrorKind::UnexpectedTopLevelStatement,
                span: Some(expr.span),
            });
        }

        self.scoper.push();
        // Define builtins
        self.define_builtins();

        self.scope(scope)?;
        self.scoper.pop();

        Ok(self.module)
    }

    fn define_builtins(&mut self) {
        self.define_builtin_type("Int", Type::Primitive(PrimitiveType::Integer));
        self.define_builtin_type("Float", Type::Primitive(PrimitiveType::Float));
        self.define_builtin_type("String", Type::Primitive(PrimitiveType::String));
        self.define_builtin_type("Bool", Type::Primitive(PrimitiveType::Boolean));
        self.define_builtin_func(
            "print",
            Builtin::Print,
            Some(Type::Primitive(PrimitiveType::String)),
            Type::Tuple(Box::new([])),
        );
        self.define_builtin_func(
            "println",
            Builtin::Println,
            Some(Type::Primitive(PrimitiveType::String)),
            Type::Tuple(Box::new([])),
        );
        self.define_builtin_func(
            "input",
            Builtin::Input,
            Some(Type::Tuple(Box::new([]))),
            Type::Primitive(PrimitiveType::String),
        );
        self.define_builtin_func(
            "itoa",
            Builtin::Itoa,
            Some(Type::Primitive(PrimitiveType::Integer)),
            Type::Primitive(PrimitiveType::String),
        );
    }

    fn define_builtin_type(&mut self, name: &str, kind: Type<'s>) {
        let name = self.interner.intern(name.into());
        self.builtin_types
            .insert(self.scoper.new_symbol(name), kind);
    }

    fn define_builtin_func(&mut self, name: &str, e: Builtin, arg: Option<Type<'s>>, ret: Type<'s>) {
        let name = self.interner.intern(name.into());
        self.module
            .builtin_funcs
            .insert(self.scoper.new_symbol(name), (e, arg, (ret)));
    }

    fn def(
        &mut self,
        init_span: Span,
        name: Intern<'s>,
        spec: bool,
        arg: Option<&parser::Expr<'s>>,
        body: &parser::Expr<'s>,
        ty: Option<&parser::Expr<'s>>,
    ) -> Result<'s, ()> {
        let sym = self.scoper.new_symbol(name);

        let arg = if let Some(arg) = arg {
            Some(self.pattern(arg, None)?)
        } else {
            None
        };

        let ty_span = ty.map(|ty| ty.span);
        let ty = if let Some(ty) = ty {
            let ty = self.type_(&ty)?;

            self.def_types.insert(
                sym,
                Type::Function(
                    arg.as_ref().map(|a| Box::new(a.ty.clone())),
                    Box::new(ty.clone()),
                ),
            );

            Some(ty)
        } else {
            None
        };

        let body = {
            let mut body = self.expr(body)?;
            if let (Some(ty), Some(ty_span)) = (ty, ty_span) {
                if body.ty.is_subtype(&ty) {
                    body.ty = ty
                } else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::TypeAssertionConflict,
                        span: Some(ty_span),
                    });
                }
            }

            body
        };

        self.module.defs.insert(
            sym,
            Def {
                decl_span: init_span,
                spec,
                name,
                ty: Type::Function(
                    arg.as_ref().map(|a| Box::new(a.ty.clone())),
                    Box::new(body.ty.clone()),
                ),
                arg,
                body,
            },
        );

        Ok(())
    }

    fn typedef(
        &mut self,
        init_span: Span,
        name: Intern<'s>,
        value: &parser::Expr<'s>,
    ) -> Result<'s, ()> {
        let sym = self.scoper.new_symbol(name);
        let type_ = self.type_(value)?;
        self.module.typedefs.insert(
            sym,
            TypeDef {
                decl_span: init_span,
                name,
                inner: type_,
            },
        );
        Ok(())
    }

    fn scope(&mut self, scope: &parser::Scope<'s>) -> Result<'s, Scope<'s>> {
        self.scoper.push();

        // TODO: traverse in topological order
        for def in &*scope.defs {
            match &def.value.kind {
                parser::ExprKind::Abstract {
                    arg,
                    spec,
                    ty,
                    body,
                } => self.def(
                    def.decl_span,
                    def.name,
                    *spec,
                    arg.as_deref(),
                    &*body,
                    ty.as_deref(),
                )?,
                _ => self.typedef(def.decl_span, def.name, &def.value)?,
            }
        }

        let mut exprs = Vec::new();
        for expr in &*scope.exprs {
            exprs.push(self.expr(expr)?)
        }

        self.scoper.pop();

        Ok(Scope {
            exprs: exprs.into_boxed_slice(),
            discard: scope.discard,
        })
    }

    fn expr(&mut self, expr: &parser::Expr<'s>) -> Result<'s, Expr<'s>> {
        let (kind, ty) = match &expr.kind {
            parser::ExprKind::Scope(scope) => {
                let scope = self.scope(scope)?;

                match (scope.discard, scope.exprs.last()) {
                    (false, Some(last)) => {
                        let last_ty = last.ty.clone();
                        (ExprKind::Scope(scope), last_ty)
                    }
                    _ => (ExprKind::Scope(scope), Type::Tuple(Box::new([]))),
                }
            }
            &parser::ExprKind::Abstract {
                spec,
                ref arg,
                ref body,
                ref ty,
            } => {
                let arg = if let Some(arg) = arg {
                    Some(self.pattern(arg, None)?)
                } else {
                    None
                };
                let body = Box::new({
                    let mut body = self.expr(body)?;
                    if let Some(ty) = ty {
                        let ty_span = ty.span;
                        let ty = self.type_(&ty)?;
                        if body.ty.is_subtype(&ty) {
                            body.ty = ty
                        } else {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::TypeAssertionConflict,
                                span: Some(ty_span),
                            });
                        }
                    }

                    body
                });

                let arg_ty = arg.as_ref().map(|a| Box::new(a.ty.clone()));
                let body_ty = Box::new(body.ty.clone());

                (
                    ExprKind::Abstract { spec, arg, body },
                    Type::Function(arg_ty, body_ty),
                )
            }
            parser::ExprKind::For {
                init,
                cond,
                afterthought,
                body,
            } => {
                self.scoper.push();

                let init = if let Some(init) = init {
                    Some(Box::new(self.expr(init)?))
                } else {
                    None
                };
                let cond = Box::new(self.expr(cond)?);
                if !cond.ty.is_bool() {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidType,
                        span: Some(cond.span),
                    });
                }
                let afterthought = if let Some(afterthought) = afterthought {
                    Some(Box::new(self.expr(afterthought)?))
                } else {
                    None
                };
                let body = Box::new(self.expr(body)?);

                self.scoper.pop();

                if !body.ty.is_subtype(&(Type::Tuple(Box::new([])))) {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidType,
                        span: Some(expr.span),
                    });
                }

                (
                    ExprKind::For {
                        init,
                        cond,
                        afterthought,
                        body,
                    },
                    Type::Tuple(Box::new([])),
                )
            }
            parser::ExprKind::Case {
                cond,
                on_true,
                on_false,
            } => {
                self.scoper.push();
                let cond = Box::new(self.expr(cond)?);
                if !cond.ty.is_bool() {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidType,
                        span: Some(cond.span),
                    });
                }
                let on_true = Box::new(self.expr(on_true)?);
                self.scoper.pop();

                if let Some(on_false) = on_false {
                    let on_false = self.expr(&on_false)?;

                    let Some(widened) = on_true.ty.widen(&on_false.ty) else {
                        return Err(ReifyError {
                            kind: ReifyErrorKind::InvalidType,
                            span: Some(expr.span),
                        });
                    };

                    (
                        ExprKind::Case {
                            cond,
                            on_true,
                            on_false: Some(Box::new(on_false)),
                        },
                        widened,
                    )
                } else {
                    (
                        ExprKind::Case {
                            cond,
                            on_true,
                            on_false: None,
                        },
                        Type::Tuple(Box::new([])),
                    )
                }
            }
            parser::ExprKind::Tuple { items } => {
                let mut reified_items = Vec::with_capacity(items.len());
                let mut tys = Vec::with_capacity(items.len());
                for item in &**items {
                    let reified = self.expr(&item)?;
                    tys.push(reified.ty.clone());
                    reified_items.push(reified);
                }

                (
                    ExprKind::Tuple(reified_items.into_boxed_slice()),
                    Type::Tuple(tys.into_boxed_slice()),
                )
            }
            parser::ExprKind::Assert { expr, ty } => {
                let expr = self.expr(expr)?;
                let asserted = self.type_(ty)?;
                if expr.ty.is_subtype(&asserted) {
                    (expr.kind, expr.ty)
                } else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidType,
                        span: Some(expr.span),
                    });
                }
            }
            &parser::ExprKind::Binary(BinOp::Eq, ref a, ref b) => {
                let kind = match (Self::has_solve(a)?, Self::has_solve(b)?) {
                    (true, true) => {
                        return Err(ReifyError {
                            kind: ReifyErrorKind::ImpossibleSolve,
                            span: Some(expr.span),
                        })
                    }
                    (true, false) => {
                        let b = Box::new(self.expr(b)?);
                        ExprKind::StructuralEq(Box::new(self.pattern(a, Some(&b.ty))?), b)
                    }
                    (false, true) => {
                        let a = Box::new(self.expr(a)?);
                        ExprKind::StructuralEq(Box::new(self.pattern(b, Some(&a.ty))?), a)
                    }
                    (false, false) => {
                        let a = Box::new(self.expr(a)?);
                        let b = Box::new(self.expr(b)?);

                        if a.ty.widen(&b.ty).is_none() {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }

                        ExprKind::Binary(BinOp::Eq, a, b)
                    }
                };

                (kind, Type::Primitive(PrimitiveType::Boolean))
            }
            &parser::ExprKind::Binary(op, ref a, ref b) => {
                let a = Box::new(self.expr(a)?);
                let b = Box::new(self.expr(b)?);

                let ty = match op {
                    BinOp::Eq | BinOp::Neq => {
                        if a.ty.widen(&b.ty).is_none() {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }

                        Type::Primitive(PrimitiveType::Boolean)
                    }
                    BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq => {
                        if !a.ty.is_int() && !a.ty.is_float() {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }
                        if !b.ty.is_int() && !b.ty.is_float() {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }
                        if a.ty != b.ty {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }

                        Type::Primitive(PrimitiveType::Boolean)
                    }
                    BinOp::BitOr
                    | BinOp::BitXor
                    | BinOp::BitAnd
                    | BinOp::Shl
                    | BinOp::Shr
                    | BinOp::Mod => {
                        if !a.ty.is_int() || !b.ty.is_int() {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }

                        Type::Primitive(PrimitiveType::Integer)
                    }
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        if !a.ty.is_int() && !a.ty.is_float() {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }
                        if !b.ty.is_int() && !b.ty.is_float() {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }
                        if a.ty != b.ty {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }

                        a.ty.clone()
                    }
                    _ => todo!(),
                };

                (ExprKind::Binary(op, a, b), ty)
            }
            &parser::ExprKind::Unary(op, ref a) => {
                let a = Box::new(self.expr(a)?);

                let ty = match op {
                    UnOp::Neg => {
                        if !a.ty.is_int() && !a.ty.is_float() {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }

                        a.ty.clone()
                    }
                    UnOp::Not => {
                        if !a.ty.is_bool() {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }

                        a.ty.clone()
                    }
                };

                (ExprKind::Unary(op, a), ty)
            }
            parser::ExprKind::Apply(a, b) => {
                let a = Box::new(self.expr(a)?);
                let b = Box::new(self.expr(b)?);

                match &a.ty {
                    Type::Function(param, ret) => {
                        let Some(param) = param else {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        };

                        if b.ty.is_subtype(&param) {
                            let ret = ret.deref().clone();
                            (ExprKind::Call(a, b), ret)
                        } else {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }
                    }
                    &Type::Constructor(sym) => {
                        let typedef = self.module.typedefs.get(&sym).unwrap();
                        if b.ty.is_subtype(&typedef.inner) {
                            (ExprKind::Construct(sym, b), Type::Symbol(sym))
                        } else {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        }
                    }
                    _ => {
                        return Err(ReifyError {
                            kind: ReifyErrorKind::InvalidType,
                            span: Some(expr.span),
                        })
                    }
                }
            }
            &parser::ExprKind::Solve(marker, _) => {
                return Err(ReifyError {
                    kind: ReifyErrorKind::UnexpectedMarker(marker),
                    span: Some(expr.span),
                })
            }
            parser::ExprKind::Variant(items) => match &**items {
                [item] => {
                    if let Some(value) = &item.value {
                        let value = self.expr(&value)?;
                        let ty = Type::Variant(Box::new([VariantItemType {
                            name: item.name,
                            inner: Some(value.ty.clone()),
                        }]));

                        (ExprKind::Variant(item.name, Some(Box::new(value))), ty)
                    } else {
                        let ty = Type::Variant(Box::new([VariantItemType {
                            name: item.name,
                            inner: None,
                        }]));

                        (ExprKind::Variant(item.name, None), ty)
                    }
                }
                _ => {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidVariant,
                        span: Some(expr.span),
                    })
                }
            },
            &parser::ExprKind::Name(name) => {
                if let Some(sym) = self.scoper.lookup(name) {
                    let kind = ExprKind::Symbol(sym);
                    let ty = if let Some(local) = self.module.locals.get(&sym) {
                        local.ty.clone()
                    } else if let Some(def) = self.module.defs.get(&sym) {
                        def.ty.clone()
                    } else if let Some(def_ty) = self.def_types.get(&sym) {
                        def_ty.clone()
                    } else if let Some(_) = self.module.typedefs.get(&sym) {
                        Type::Constructor(sym)
                    } else if let Some((_, arg, ret)) = self.module.builtin_funcs.get(&sym) {
                        Type::Function(arg.clone().map(Box::new), Box::new(ret.clone()))
                    } else {
                        return Err(ReifyError {
                            kind: ReifyErrorKind::InvalidType,
                            span: Some(expr.span),
                        });
                    };

                    (kind, ty)
                } else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::UndefinedSymbol(name),
                        span: Some(expr.span),
                    });
                }
            }
            &parser::ExprKind::Literal(lit) => {
                let kind = ExprKind::Literal(lit);
                let ty = match lit {
                    Literal::Float(_) => Type::Primitive(PrimitiveType::Float),
                    Literal::Integer(_) => Type::Primitive(PrimitiveType::Integer),
                    Literal::String(_) => Type::Primitive(PrimitiveType::String),
                    Literal::Boolean(_) => Type::Primitive(PrimitiveType::Boolean),
                };

                (kind, ty)
            }
        };

        Ok(Expr {
            kind,
            span: expr.span,
            ty,
        })
    }

    fn type_(&mut self, expr: &parser::Expr<'s>) -> Result<'s, Type<'s>> {
        let kind = match &expr.kind {
            parser::ExprKind::Abstract { arg, body, .. } => Type::Function(
                if let Some(arg) = arg {
                    Some(Box::new(self.type_(arg)?))
                } else {
                    None
                },
                Box::new(self.type_(body)?),
            ),
            parser::ExprKind::Tuple { items } => Type::Tuple({
                let mut new_items = Vec::with_capacity(items.len());
                for item in &**items {
                    new_items.push(self.type_(item)?);
                }

                new_items.into_boxed_slice()
            }),
            parser::ExprKind::Apply(a, b) => {
                let parser::ExprKind::Name(name) = &a.kind else {
                    return Err(ReifyError { kind: ReifyErrorKind::InvalidType, span: Some(expr.span) })
                };
                let Some(sym) = self.scoper.lookup(*name) else {
                    return Err(ReifyError { kind: ReifyErrorKind::UndefinedSymbol(*name), span: Some(a.span) })
                };

                let arg_type = self.type_(b)?;
                let Some(typedef) = self.module.typedefs.get(&sym) else {
                    return Err(ReifyError { kind: ReifyErrorKind::InvalidType, span: Some(expr.span) })
                };

                if !arg_type.is_subtype(&typedef.inner) {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidType,
                        span: Some(expr.span),
                    });
                }

                Type::Symbol(sym)
            }
            parser::ExprKind::Name(name) => {
                if let Some(symbol) = self.scoper.lookup(*name) {
                    if let Some(builtin) = self.builtin_types.get(&symbol) {
                        builtin.clone()
                    } else {
                        Type::Symbol(symbol)
                    }
                } else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::UndefinedSymbol(*name),
                        span: Some(expr.span),
                    });
                }
            }
            parser::ExprKind::Variant(items) => Type::Variant({
                let mut new_items = Vec::with_capacity(items.len());
                for item in &**items {
                    new_items.push(VariantItemType {
                        name: item.name,
                        inner: if let Some(value) = &item.value {
                            Some(self.type_(&value)?)
                        } else {
                            None
                        },
                    })
                }

                new_items.into_boxed_slice()
            }),
            _ => {
                return Err(ReifyError {
                    kind: ReifyErrorKind::InvalidType,
                    span: Some(expr.span),
                })
            }
        };

        Ok(kind)
    }

    fn pattern(
        &mut self,
        expr: &parser::Expr<'s>,
        ty: Option<&Type<'s>>,
    ) -> Result<'s, Pattern<'s>> {
        let (kind, ty) = match &expr.kind {
            parser::ExprKind::Apply(a, b) => {
                let a_ty = if let Some(ty) = ty {
                    let Type::Symbol(sym) = ty else {
                        return Err(ReifyError {
                            kind: ReifyErrorKind::InvalidType,
                            span: Some(a.span),
                        });
                    };

                    Some(Type::Constructor(*sym))
                } else {
                    None
                };

                let a = self.pattern(a, a_ty.as_ref())?;
                let Type::Constructor(sym) = a.ty else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidType,
                        span: Some(a.span),
                    });
                };

                let typedef = self.module.typedefs.get(&sym).unwrap();
                let b = self.pattern(b, Some(&typedef.inner.clone()))?;

                (
                    PatternKind::Apply(Box::new(a), Box::new(b)),
                    Type::Symbol(sym),
                )
            }
            parser::ExprKind::Variant(items) => match &**items {
                [item] => {
                    let value_ty = if let Some(ty) = ty {
                        let Type::Variant(items) = ty else {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        };

                        let Some(variant) = items.iter().filter(|it| it.name == item.name).next() else {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::InvalidType,
                                span: Some(expr.span),
                            });
                        };

                        variant.inner.clone()
                    } else {
                        None
                    };

                    if let Some(value) = &item.value {
                        let value = self.pattern(&value, value_ty.as_ref())?;
                        let inner_ty = Some(value.ty.clone());

                        (
                            PatternKind::Variant(item.name, Some(Box::new(value))),
                            Type::Variant(Box::new([VariantItemType {
                                name: item.name,
                                inner: inner_ty,
                            }])),
                        )
                    } else {
                        (
                            PatternKind::Variant(item.name, None),
                            Type::Variant(Box::new([VariantItemType {
                                name: item.name,
                                inner: None,
                            }])),
                        )
                    }
                }
                _ => {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidVariant,
                        span: Some(expr.span),
                    })
                }
            },
            parser::ExprKind::Tuple { items } => {
                if let Some(ty) = ty {
                    let Type::Tuple(ty_items) = ty else {
                        return Err(ReifyError {
                            kind: ReifyErrorKind::InvalidType,
                            span: Some(expr.span),
                        });
                    };

                    if items.len() != ty_items.len() {
                        return Err(ReifyError {
                            kind: ReifyErrorKind::InvalidType,
                            span: Some(expr.span),
                        });
                    }

                    let mut reified_items = Vec::with_capacity(items.len());
                    for (it, ty_it) in items.iter().zip(ty_items.iter()) {
                        reified_items.push(self.pattern(it, Some(ty_it))?);
                    }

                    (
                        PatternKind::Tuple(reified_items.into_boxed_slice()),
                        ty.clone(),
                    )
                } else {
                    let mut reified_items = Vec::with_capacity(items.len());
                    let mut ty_its = Vec::with_capacity(items.len());
                    for item in &**items {
                        let item = self.pattern(&item, None)?;
                        ty_its.push(item.ty.clone());
                        reified_items.push(item);
                    }

                    (
                        PatternKind::Tuple(reified_items.into_boxed_slice()),
                        Type::Tuple(ty_its.into_boxed_slice()),
                    )
                }
            }
            parser::ExprKind::Assert { expr, ty } => {
                let ty = self.type_(ty)?;
                let pat = self.pattern(expr, Some(&ty))?;

                (pat.kind, pat.ty)
            }
            &parser::ExprKind::Solve(marker @ (SolveMarker::Val | SolveMarker::Var), name) => {
                let Some(ty) = ty else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidType,
                        span: Some(expr.span),
                    });
                };

                let sym = self.scoper.new_symbol(name);

                self.module.locals.insert(
                    sym,
                    Local {
                        decl_span: expr.span,
                        name,
                        mutable: marker == SolveMarker::Var,
                        ty: ty.clone(),
                    },
                );

                (PatternKind::Solve(marker, sym), ty.clone())
            }
            &parser::ExprKind::Solve(SolveMarker::Set, name) => {
                let Some(sym) = self.scoper.lookup(name) else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::UndefinedSymbol(name),
                        span: Some(expr.span),
                    });
                };

                let Some(local) = self.module.locals.get(&sym) else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidPattern,
                        span: Some(expr.span),
                    });
                };

                (PatternKind::Solve(SolveMarker::Set, sym), local.ty.clone())
            }
            &parser::ExprKind::Name(name) => {
                let Some(sym) = self.scoper.lookup(name) else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::UndefinedSymbol(name),
                        span: Some(expr.span),
                    });
                };

                if self.module.typedefs.get(&sym).is_none() {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidPattern,
                        span: Some(expr.span),
                    });
                }

                (PatternKind::Symbol(sym), Type::Constructor(sym))
            }
            _ => {
                return Err(ReifyError {
                    kind: ReifyErrorKind::InvalidPattern,
                    span: Some(expr.span),
                })
            }
        };

        Ok(Pattern {
            kind,
            span: expr.span,
            ty,
        })
    }

    fn has_solve(expr: &parser::Expr) -> Result<'s, bool> {
        Ok(match &expr.kind {
            parser::ExprKind::Tuple { items } => {
                for item in items.iter() {
                    if Self::has_solve(item)? {
                        return Ok(true);
                    }
                }

                false
            }
            parser::ExprKind::Variant(items) => {
                for item in items.iter() {
                    if let Some(value) = &item.value {
                        if Self::has_solve(value)? {
                            return Ok(true);
                        }
                    }
                }

                false
            }
            parser::ExprKind::Assert { expr, .. } => Self::has_solve(&expr)?,
            parser::ExprKind::Apply(a, b) => Self::has_solve(&a)? || Self::has_solve(&b)?,
            parser::ExprKind::Solve(_, _) => true,
            _ => false,
        })
    }
}
