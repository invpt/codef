//! Resolves references, translates the AST to more of a semantics tree.

use std::num::NonZeroUsize;

mod rst;
mod scoper;
pub use rst::*;
use rustc_hash::FxHashMap;

use crate::{
    parser,
    string_storage::{Intern, StringInterner, StringStorage},
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
        builtin_types: FxHashMap::default(),
    }
    .reify(expr)
}

struct Reifier<'s> {
    interner: &'s mut StringInterner<'s>,
    scoper: Scoper<'s>,
    module: Module<'s>,
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
        self.define_builtin_type("Int", TypeKind::Primitive(PrimitiveType::Integer));
        self.define_builtin_type("Float", TypeKind::Primitive(PrimitiveType::Float));
        self.define_builtin_type("String", TypeKind::Primitive(PrimitiveType::String));
        self.define_builtin_type("Bool", TypeKind::Primitive(PrimitiveType::Boolean));
        self.define_builtin_func("print", Some(TypeKind::Primitive(PrimitiveType::String)), TypeKind::Tuple(Box::new([])));
        self.define_builtin_func("println", Some(TypeKind::Primitive(PrimitiveType::String)), TypeKind::Tuple(Box::new([])));
        self.define_builtin_func("input", Some(TypeKind::Tuple(Box::new([]))), TypeKind::Primitive(PrimitiveType::String));
        self.define_builtin_func("itoa", Some(TypeKind::Primitive(PrimitiveType::Integer)), TypeKind::Primitive(PrimitiveType::String));
    }

    fn define_builtin_type(&mut self, name: &str, kind: TypeKind<'s>) {
        let name = self.interner.intern(name.into());
        self.builtin_types.insert(self.scoper.new_symbol(name), Type { span: None, kind });
    }

    fn define_builtin_func(&mut self, name: &str, arg: Option<TypeKind<'s>>, ret: TypeKind<'s>) {
        let name = self.interner.intern(name.into());
        self.module.builtin_funcs.insert(self.scoper.new_symbol(name), (arg.map(Type::inferred), Type::inferred(ret)));
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
            Some(self.pattern(arg)?)
        } else {
            None
        };

        let body = if let parser::ExprKind::Scope(scope) = &body.kind {
            Expr {
                kind: ExprKind::Scope(self.scope(scope)?),
                span: body.span,
                ty: if let Some(ty) = ty {
                    Some(self.type_(ty)?)
                } else {
                    None
                },
            }
        } else {
            let mut body = self.expr(body)?;
            if let Some(ty) = ty {
                let ty_span = ty.span;
                let ty = self.type_(&ty)?;
                if let Some(bodyty) = body.ty {
                    if bodyty.is_subtype(&ty) {
                        body.ty = Some(ty)
                    } else {
                        return Err(ReifyError {
                            kind: ReifyErrorKind::TypeAssertionConflict,
                            span: Some(ty_span),
                        });
                    }
                } else {
                    body.ty = Some(ty)
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

    fn type_(&mut self, expr: &parser::Expr<'s>) -> Result<'s, Type<'s>> {
        let kind = match &expr.kind {
            parser::ExprKind::Abstract { arg, body, .. } => TypeKind::Function(
                if let Some(arg) = arg {
                    Some(Box::new(self.type_(arg)?))
                } else {
                    None
                },
                Box::new(self.type_(body)?),
            ),
            parser::ExprKind::Tuple { items } => TypeKind::Tuple({
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

                TypeKind::Symbol(sym)
            }
            parser::ExprKind::Name(name) => {
                if let Some(symbol) = self.scoper.lookup(*name) {
                    if let Some(builtin) = self.builtin_types.get(&symbol) {
                        builtin.kind.clone()
                    } else {
                        TypeKind::Symbol(symbol)
                    }
                } else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::UndefinedSymbol(*name),
                        span: Some(expr.span),
                    });
                }
            }
            parser::ExprKind::Variant(items) => TypeKind::Variant({
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

        Ok(Type {
            kind,
            span: Some(expr.span),
        })
    }

    fn pattern(&mut self, expr: &parser::Expr<'s>) -> Result<'s, Pattern<'s>> {
        let mut type_ = None;
        let kind = match &expr.kind {
            parser::ExprKind::Apply(a, b) => {
                PatternKind::Apply(Box::new(self.pattern(a)?), Box::new(self.pattern(b)?))
            }
            parser::ExprKind::Variant(items) => match items.len() {
                1 => match &items[0] {
                    parser::VariantItem {
                        name,
                        value: Some(value),
                        ..
                    } => PatternKind::Variant(*name, Some(Box::new(self.pattern(value)?))),
                    parser::VariantItem {
                        name, value: None, ..
                    } => PatternKind::Variant(*name, None),
                },
                _ => {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidVariant,
                        span: Some(expr.span),
                    })
                }
            },
            parser::ExprKind::Tuple { items } => PatternKind::Tuple({
                let mut reified_items = Vec::with_capacity(items.len());
                for item in &**items {
                    reified_items.push(self.pattern(&item)?)
                }
                reified_items.into_boxed_slice()
            }),
            parser::ExprKind::Assert { expr, ty } => {
                type_ = Some(self.type_(ty)?);
                self.pattern(expr)?.kind
            }
            &parser::ExprKind::Solve(marker, name) => PatternKind::Solve(
                marker,
                match marker {
                    SolveMarker::Set => match self.scoper.lookup(name) {
                        Some(sym) => sym,
                        None => {
                            return Err(ReifyError {
                                kind: ReifyErrorKind::UndefinedSymbol(name),
                                span: Some(expr.span),
                            })
                        }
                    },
                    SolveMarker::Val | SolveMarker::Var => {
                        let sym = self.scoper.new_symbol(name);
                        self.module.locals.insert(
                            sym,
                            Local {
                                decl_span: expr.span,
                                name,
                                mutable: marker == SolveMarker::Var,
                            },
                        );
                        sym
                    }
                },
            ),
            &parser::ExprKind::Name(name) => PatternKind::Symbol({
                if let Some(symbol) = self.scoper.lookup(name) {
                    symbol
                } else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::UndefinedSymbol(name),
                        span: Some(expr.span),
                    });
                }
            }),
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
            ty: type_,
        })
    }

    fn expr(&mut self, expr: &parser::Expr<'s>) -> Result<'s, Expr<'s>> {
        let mut type_ = None;
        let kind = match &expr.kind {
            parser::ExprKind::Scope(scope) => ExprKind::Scope(self.scope(scope)?),
            parser::ExprKind::Abstract {
                spec,
                arg,
                body,
                ty,
            } => ExprKind::Abstract {
                spec: *spec,
                arg: if let Some(arg) = arg {
                    Some(self.pattern(arg)?)
                } else {
                    None
                },
                body: Box::new({
                    let mut body = self.expr(body)?;
                    if let Some(ty) = ty {
                        let ty_span = ty.span;
                        let ty = self.type_(&ty)?;
                        if let Some(bodyty) = body.ty {
                            if bodyty.is_subtype(&ty) {
                                body.ty = Some(ty)
                            } else {
                                return Err(ReifyError {
                                    kind: ReifyErrorKind::TypeAssertionConflict,
                                    span: Some(ty_span),
                                });
                            }
                        } else {
                            body.ty = Some(ty)
                        }
                    }

                    body
                }),
            },
            parser::ExprKind::For {
                init,
                cond,
                afterthought,
                body,
            } => {
                self.scoper.push();
                let for_ = ExprKind::For {
                    init: if let Some(init) = init {
                        Some(Box::new(self.expr(init)?))
                    } else {
                        None
                    },
                    cond: Box::new(self.expr(cond)?),
                    afterthought: if let Some(afterthought) = afterthought {
                        Some(Box::new(self.expr(afterthought)?))
                    } else {
                        None
                    },
                    body: Box::new(self.expr(body)?),
                };
                self.scoper.pop();
                for_
            }
            parser::ExprKind::Case {
                cond,
                on_true,
                on_false,
            } => ExprKind::Case {
                cond: Box::new({
                    self.scoper.push();
                    self.expr(cond)?
                }),
                on_true: Box::new({
                    let on_true = self.expr(on_true)?;
                    self.scoper.pop();
                    on_true
                }),
                on_false: if let Some(on_false) = on_false {
                    Some(Box::new(self.expr(on_false)?))
                } else {
                    None
                },
            },
            parser::ExprKind::Tuple { items } => ExprKind::Tuple {
                items: {
                    let mut reified_items = Vec::with_capacity(items.len());
                    for item in &**items {
                        reified_items.push(self.expr(&item)?)
                    }
                    reified_items.into_boxed_slice()
                },
            },
            parser::ExprKind::Assert { expr, ty } => {
                type_ = Some(self.type_(ty)?);
                self.expr(expr)?.kind
            }
            &parser::ExprKind::Binary(op, ref a, ref b) => match op {
                BinOp::Eq => {
                    let ahs = Self::has_solve(a)?;
                    let bhs = Self::has_solve(b)?;
                    if ahs && bhs {
                        return Err(ReifyError {
                            kind: ReifyErrorKind::ImpossibleSolve,
                            span: Some(expr.span),
                        });
                    } else if ahs {
                        let pat = self.pattern(a)?;
                        let val = self.expr(b)?;
                        ExprKind::StructuralEq(Box::new(pat), Box::new(val))
                    } else if bhs {
                        let pat = self.pattern(b)?;
                        let val = self.expr(a)?;
                        ExprKind::StructuralEq(Box::new(pat), Box::new(val))
                    } else {
                        ExprKind::Binary(op, Box::new(self.expr(a)?), Box::new(self.expr(b)?))
                    }
                }
                _ => ExprKind::Binary(op, Box::new(self.expr(a)?), Box::new(self.expr(b)?)),
            },
            &parser::ExprKind::Unary(op, ref a) => ExprKind::Unary(op, Box::new(self.expr(a)?)),
            parser::ExprKind::Apply(a, b) => {
                ExprKind::Apply(Box::new(self.expr(a)?), Box::new(self.expr(b)?))
            }
            &parser::ExprKind::Solve(marker, _) => {
                return Err(ReifyError {
                    kind: ReifyErrorKind::UnexpectedMarker(marker),
                    span: Some(expr.span),
                })
            }
            parser::ExprKind::Variant(items) => match items.len() {
                1 => match &items[0] {
                    parser::VariantItem {
                        name,
                        value: Some(value),
                        ..
                    } => ExprKind::Variant(*name, Some(Box::new(self.expr(value)?))),
                    parser::VariantItem {
                        name, value: None, ..
                    } => ExprKind::Variant(*name, None),
                },
                _ => {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::InvalidVariant,
                        span: Some(expr.span),
                    })
                }
            },
            &parser::ExprKind::Name(name) => ExprKind::Symbol({
                if let Some(symbol) = self.scoper.lookup(name) {
                    symbol
                } else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::UndefinedSymbol(name),
                        span: Some(expr.span),
                    });
                }
            }),
            &parser::ExprKind::Literal(lit) => ExprKind::Literal(lit),
        };

        Ok(Expr {
            kind,
            span: expr.span,
            ty: type_,
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
