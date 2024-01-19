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
        } = expr
        else {
            return Err(ReifyError {
                kind: ReifyErrorKind::UnexpectedTopLevelStatement,
                span: Some(expr.span),
            });
        };
        // There cannot be any statements at the top-level
        if let [expr, ..] = &*scope.exprs {
            return Err(ReifyError {
                kind: ReifyErrorKind::UnexpectedTopLevelStatement,
                span: Some(expr.span),
            });
        }

        self.scoper.push();
        self.define_builtins();
        self.scope(scope)?;
        self.scoper.pop();

        Ok(self.module)
    }

    fn define_builtins(&mut self) {
        self.builtin_type("Int", Type::Primitive(PrimitiveType::Integer));
        self.builtin_type("Float", Type::Primitive(PrimitiveType::Float));
        self.builtin_type("String", Type::Primitive(PrimitiveType::String));
        self.builtin_type("Bool", Type::Primitive(PrimitiveType::Boolean));
        self.builtin_def(
            "print",
            Builtin::Print,
            Type::Function(
                Some(Box::new(Type::Primitive(PrimitiveType::String))),
                Box::new(Type::Tuple(Box::new([]))),
            ),
        );
        self.builtin_def(
            "println",
            Builtin::Print,
            Type::Function(
                Some(Box::new(Type::Primitive(PrimitiveType::String))),
                Box::new(Type::Tuple(Box::new([]))),
            ),
        );
        self.builtin_def(
            "input",
            Builtin::Print,
            Type::Function(
                Some(Box::new(Type::Tuple(Box::new([])))),
                Box::new(Type::Primitive(PrimitiveType::String)),
            ),
        );
        self.builtin_def(
            "itoa",
            Builtin::Print,
            Type::Function(
                Some(Box::new(Type::Primitive(PrimitiveType::Integer))),
                Box::new(Type::Primitive(PrimitiveType::String)),
            ),
        );
    }

    fn builtin_type(&mut self, name: &str, kind: Type<'s>) {
        let name = self.interner.intern(name.into());
        self.builtin_types
            .insert(self.scoper.new_symbol(name), kind);
    }

    fn builtin_def(&mut self, name: &str, which: Builtin, ty: Type<'s>) {
        let name = self.interner.intern(name.into());
        self.module
            .builtins
            .insert(self.scoper.new_symbol(name), (which, ty));
    }

    fn scope(&mut self, scope: &parser::Scope<'s>) -> Result<'s, Scope<'s>> {
        self.scoper.push();

        for def in &*scope.typedefs {
            self.typedef(def.decl_span, def.name, &def.value)?;
        }

        let mut assigned_symbols = Vec::with_capacity(scope.defs.len());
        for def in &*scope.defs {
            let sym = self.scoper.new_symbol(def.name);
            self.def_types.insert(sym, Type::Unknown);
            assigned_symbols.push(sym);
        }
        for (sym, def) in assigned_symbols.into_iter().zip(scope.defs.iter()) {
            self.def(sym, &def)?;
        }

        let mut exprs = Vec::with_capacity(scope.exprs.len());
        for expr in &*scope.exprs {
            exprs.push(self.expr(expr, &Type::Unknown)?)
        }

        self.scoper.pop();

        Ok(Scope {
            exprs: exprs.into_boxed_slice(),
            discard: scope.discard,
        })
    }

    fn def(&mut self, sym: Symbol, def: &parser::Def<'s>) -> Result<'s, ()> {
        let body = self.expr(&def.value, &Type::Unknown)?;

        self.def_types.insert(sym, body.ty.clone());
        self.module.defs.insert(
            sym,
            Def {
                decl_span: def.decl_span,
                name: def.name,
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
        let inner = self.type_(value)?;
        self.module.defs.insert(
            sym,
            Def {
                decl_span: init_span,
                name,
                body: Expr {
                    kind: ExprKind::Constructor(sym),
                    span: value.span,
                    ty: Type::Function(Some(Box::new(inner)), Box::new(Type::Instance(sym))),
                },
            },
        );
        Ok(())
    }

    fn expr(&mut self, expr: &parser::Expr<'s>, superty: &Type<'s>) -> Result<'s, Expr<'s>> {
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
            parser::ExprKind::Abstract {
                spec,
                arg,
                body,
                ret,
            } => {
                let arg = if let Some(arg) = arg {
                    Some(self.pattern(arg, &Type::Unknown)?)
                } else {
                    None
                };

                let body_ty = if let Some(ret) = ret {
                    self.type_(&ret)?
                } else {
                    Type::Unknown
                };

                let body = self.expr(body, &body_ty)?;
                let arg_ty = arg.as_ref().map(|a| Box::new(a.ty.clone()));
                let body_ty = Box::new(body.ty.clone());

                (
                    ExprKind::Abstract {
                        spec: *spec,
                        arg,
                        body: Box::new(body),
                    },
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
                    Some(Box::new(self.expr(init, &Type::Unknown)?))
                } else {
                    None
                };
                let cond = Box::new(self.expr(cond, &Type::Primitive(PrimitiveType::Boolean))?);
                let afterthought = if let Some(afterthought) = afterthought {
                    Some(Box::new(self.expr(afterthought, &Type::Unknown)?))
                } else {
                    None
                };
                let body = Box::new(self.expr(body, &Type::Tuple(Box::new([])))?);

                self.scoper.pop();

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
                let cond = Box::new(self.expr(cond, &Type::Primitive(PrimitiveType::Boolean))?);
                let on_true = Box::new(self.expr(on_true, superty)?);
                self.scoper.pop();

                if let Some(on_false) = on_false {
                    let on_false = self.expr(&on_false, superty)?;

                    let Some(widened) = on_true.ty.widen(&on_false.ty) else {
                        return Err(ReifyError {
                            kind: dbg!(ReifyErrorKind::InvalidType),
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
                let supertys = if let Type::Tuple(tys) = superty {
                    &**tys
                } else {
                    &[]
                }
                .iter()
                .chain(std::iter::repeat(&Type::Unknown));

                let mut reified_items = Vec::with_capacity(items.len());
                let mut tys = Vec::with_capacity(items.len());
                for (item, superty) in items.iter().zip(supertys) {
                    let reified = self.expr(&item, superty)?;
                    tys.push(reified.ty.clone());
                    reified_items.push(reified);
                }

                (
                    ExprKind::Tuple(reified_items.into_boxed_slice()),
                    Type::Tuple(tys.into_boxed_slice()),
                )
            }
            parser::ExprKind::Assert { expr, ty } => {
                let asserted = self.type_(ty)?;
                let expr = self.expr(expr, &asserted)?;
                (expr.kind, expr.ty)
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
                        let b = Box::new(self.expr(b, &Type::Unknown)?);
                        ExprKind::StructuralEq(Box::new(self.pattern(a, &b.ty)?), b)
                    }
                    (false, true) => {
                        let a = Box::new(self.expr(a, &Type::Unknown)?);
                        ExprKind::StructuralEq(Box::new(self.pattern(b, &a.ty)?), a)
                    }
                    (false, false) => {
                        let a = Box::new(self.expr(a, &Type::Unknown)?);
                        let b = Box::new(self.expr(b, &Type::Unknown)?);

                        if a.ty.widen(&b.ty).is_none() {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        }

                        ExprKind::Binary(BinOp::Eq, a, b)
                    }
                };

                (kind, Type::Primitive(PrimitiveType::Boolean))
            }
            &parser::ExprKind::Binary(op, ref a, ref b) => {
                let (ty, a, b) = match op {
                    BinOp::Eq | BinOp::Neq => {
                        let a = Box::new(self.expr(a, &Type::Unknown)?);
                        let b = Box::new(self.expr(b, &Type::Unknown)?);

                        if a.ty.widen(&b.ty).is_none() {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        }

                        (Type::Primitive(PrimitiveType::Boolean), a, b)
                    }
                    BinOp::Lt | BinOp::Leq | BinOp::Gt | BinOp::Geq => {
                        let a = Box::new(self.expr(a, &Type::Unknown)?);
                        let b = Box::new(self.expr(b, &Type::Unknown)?);

                        if !a.ty.is_int() && !a.ty.is_float() {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        }
                        if !b.ty.is_int() && !b.ty.is_float() {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        }
                        if a.ty != b.ty {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        }

                        (Type::Primitive(PrimitiveType::Boolean), a, b)
                    }
                    BinOp::BitOr
                    | BinOp::BitXor
                    | BinOp::BitAnd
                    | BinOp::Shl
                    | BinOp::Shr
                    | BinOp::Mod => {
                        let a = Box::new(self.expr(a, &Type::Primitive(PrimitiveType::Integer))?);
                        let b = Box::new(self.expr(b, &Type::Primitive(PrimitiveType::Integer))?);

                        (Type::Primitive(PrimitiveType::Integer), a, b)
                    }
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        let a = Box::new(self.expr(a, superty)?);
                        let b = Box::new(self.expr(b, superty)?);

                        if !a.ty.is_int() && !a.ty.is_float() {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        }
                        if !b.ty.is_int() && !b.ty.is_float() {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        }
                        if a.ty != b.ty {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        }

                        (a.ty.clone(), a, b)
                    }
                    _ => todo!(),
                };

                (ExprKind::Binary(op, a, b), ty)
            }
            &parser::ExprKind::Unary(op, ref a) => {
                let a = Box::new(self.expr(a, &Type::Unknown)?);

                let ty = match op {
                    UnOp::Neg => {
                        if !a.ty.is_int() && !a.ty.is_float() {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        }

                        a.ty.clone()
                    }
                    UnOp::Not => {
                        if !a.ty.is_bool() {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        }

                        a.ty.clone()
                    }
                };

                (ExprKind::Unary(op, a), ty)
            }
            parser::ExprKind::Apply(a, b) => {
                if !superty.is_unknown() {
                    let b = Box::new(self.expr(b, &Type::Unknown)?);
                    let a_ty =
                        Type::Function(Some(Box::new(b.ty.clone())), Box::new(superty.clone()));
                    let a = Box::new(self.expr(&a, &a_ty)?);
                    let Type::Function(_, ret) = &a.ty else {
                        return Err(ReifyError {
                            kind: dbg!(ReifyErrorKind::InvalidType),
                            span: Some(expr.span),
                        });
                    };

                    let ret = ret.deref().clone();
                    (ExprKind::Apply(a, b), ret)
                } else {
                    let a = Box::new(self.expr(a, &Type::Unknown)?);

                    match &a.ty {
                        Type::Function(param, ret) => {
                            let Some(param) = param else {
                                return Err(ReifyError {
                                    kind: dbg!(ReifyErrorKind::InvalidType),
                                    span: Some(expr.span),
                                });
                            };

                            let b = Box::new(self.expr(b, &*param)?);

                            let ret = ret.deref().clone();
                            (ExprKind::Apply(a, b), ret)
                        }
                        Type::Unknown => (
                            ExprKind::Apply(a, Box::new(self.expr(b, &Type::Unknown)?)),
                            Type::Unknown,
                        ),
                        _ => {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            })
                        }
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
                        let value = self.expr(&value, &Type::Unknown)?;
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
                    let kind = ExprKind::Load(sym);
                    let ty = if let Some(local) = self.module.locals.get(&sym) {
                        if !superty.is_unknown() && superty.is_subtype(&local.ty) {
                            let local = self.module.locals.get_mut(&sym).unwrap();
                            local.ty = superty.clone();
                            local.ty.clone()
                        } else {
                            local.ty.clone()
                        }
                    } else if let Some(def) = self.module.defs.get(&sym) {
                        if !superty.is_unknown() && superty.is_subtype(&def.body.ty) {
                            let def = self.module.defs.get_mut(&sym).unwrap();
                            def.body.ty = superty.clone();
                            def.body.ty.clone()
                        } else {
                            def.body.ty.clone()
                        }
                    } else if let Some(def_ty) = self.def_types.get(&sym) {
                        if !superty.is_unknown() && superty.is_subtype(def_ty) {
                            let def_ty = self.def_types.get_mut(&sym).unwrap();
                            *def_ty = superty.clone();
                            def_ty.clone()
                        } else {
                            def_ty.clone()
                        }
                    } else if let Some((_, ty)) = self.module.builtins.get(&sym) {
                        ty.clone()
                    } else {
                        return Err(ReifyError {
                            kind: dbg!(ReifyErrorKind::InvalidType),
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

        if !ty.is_subtype(superty) {
            return Err(ReifyError {
                kind: dbg!(ReifyErrorKind::InvalidType),
                span: Some(expr.span),
            });
        }

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
                    return Err(ReifyError {
                        kind: dbg!(ReifyErrorKind::InvalidType),
                        span: Some(expr.span),
                    });
                };
                let Some(sym) = self.scoper.lookup(*name) else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::UndefinedSymbol(*name),
                        span: Some(a.span),
                    });
                };

                let arg_type = self.type_(b)?;
                let Some(def) = self.module.defs.get(&sym) else {
                    return Err(ReifyError {
                        kind: dbg!(ReifyErrorKind::InvalidType),
                        span: Some(expr.span),
                    });
                };
                let Expr { kind: ExprKind::Load(_), ty: Type::Function(Some(a_param), _), .. } = &def.body else {
                    return Err(ReifyError {
                        kind: dbg!(ReifyErrorKind::InvalidType),
                        span: Some(expr.span),
                    });
                };

                if !arg_type.is_subtype(&a_param) {
                    return Err(ReifyError {
                        kind: dbg!(ReifyErrorKind::InvalidType),
                        span: Some(expr.span),
                    });
                }

                Type::Instance(sym)
            }
            parser::ExprKind::Name(name) => {
                if let Some(symbol) = self.scoper.lookup(*name) {
                    if let Some(builtin) = self.builtin_types.get(&symbol) {
                        builtin.clone()
                    } else {
                        Type::Instance(symbol)
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
                    kind: dbg!(ReifyErrorKind::InvalidType),
                    span: Some(expr.span),
                })
            }
        };

        Ok(kind)
    }

    fn x() {
        let _: (i32, _) = {
            let x = unsafe { std::mem::transmute::<u32, _>(0) };
            let y = x;
            let z = y;
            (x, z)
        };
        println!("Hello, world!");
    }

    fn pattern(
        &mut self,
        expr: &parser::Expr<'s>,
        superty: &Type<'s>,
    ) -> Result<'s, Pattern<'s>> {
        let (kind, ty) = match &expr.kind {
            parser::ExprKind::Apply(a, b) => {
                let a_ty = if !superty.is_unknown() {
                    let Type::Instance(sym) = superty else {
                        return Err(ReifyError {
                            kind: dbg!(ReifyErrorKind::InvalidType),
                            span: Some(a.span),
                        });
                    };

                    let constructor = self.module.defs.get(&sym).unwrap();

                    constructor.body.ty.clone()
                } else {
                    Type::Unknown
                };

                let a = self.pattern(&a, &a_ty)?;
                let Type::Function(Some(param), ret) = &a.ty else {
                    return Err(ReifyError {
                        kind: dbg!(ReifyErrorKind::InvalidType),
                        span: Some(a.span),
                    });
                };
                let &Type::Instance(sym) = &**ret else {
                    return Err(ReifyError {
                        kind: dbg!(ReifyErrorKind::InvalidType),
                        span: Some(a.span),
                    });
                };
                let b = self.pattern(&b, &param)?;

                (
                    PatternKind::Apply(Box::new(a), Box::new(b)),
                    Type::Instance(sym),
                )
            }
            parser::ExprKind::Variant(items) => match &**items {
                [item] => {
                    let value_ty = if !superty.is_unknown() {
                        let Type::Variant(items) = superty else {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        };

                        let Some(variant) = items.iter().filter(|it| it.name == item.name).next()
                        else {
                            return Err(ReifyError {
                                kind: dbg!(ReifyErrorKind::InvalidType),
                                span: Some(expr.span),
                            });
                        };

                        variant.inner.as_ref().unwrap_or(&Type::Unknown).clone()
                    } else {
                        Type::Unknown
                    };

                    if let Some(value) = &item.value {
                        let value = self.pattern(&value, &value_ty)?;
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
                if !superty.is_unknown() {
                    let Type::Tuple(ty_items) = superty else {
                        return Err(ReifyError {
                            kind: dbg!(ReifyErrorKind::InvalidType),
                            span: Some(expr.span),
                        });
                    };

                    if items.len() != ty_items.len() {
                        return Err(ReifyError {
                            kind: dbg!(ReifyErrorKind::InvalidType),
                            span: Some(expr.span),
                        });
                    }

                    let mut reified_items = Vec::with_capacity(items.len());
                    for (it, ty_it) in items.iter().zip(ty_items.iter()) {
                        reified_items.push(self.pattern(it, ty_it)?);
                    }

                    (
                        PatternKind::Tuple(reified_items.into_boxed_slice()),
                        superty.clone(),
                    )
                } else {
                    let mut reified_items = Vec::with_capacity(items.len());
                    let mut ty_its = Vec::with_capacity(items.len());
                    for item in &**items {
                        let item = self.pattern(&item, &Type::Unknown)?;
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
                let pat = self.pattern(expr, &ty)?;

                (pat.kind, pat.ty)
            }
            &parser::ExprKind::Solve(marker @ (SolveMarker::Val | SolveMarker::Var), name) => {
                if superty.is_unknown() {
                    return Err(ReifyError {
                        kind: dbg!(ReifyErrorKind::InvalidType),
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
                        ty: superty.clone(),
                    },
                );

                (PatternKind::Solve(marker, sym), superty.clone())
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

                if !local.mutable {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidPattern,
                        span: Some(expr.span),
                    });
                }

                (PatternKind::Solve(SolveMarker::Set, sym), local.ty.clone())
            }
            &parser::ExprKind::Name(name) => {
                let Some(sym) = self.scoper.lookup(name) else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::UndefinedSymbol(name),
                        span: Some(expr.span),
                    });
                };

                (
                    PatternKind::Symbol(sym),
                    self.module.defs.get(&sym).unwrap().body.ty.clone(),
                )
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
