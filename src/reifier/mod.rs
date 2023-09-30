//! Resolves references, translates the AST to more of a semantics tree.

use std::{collections::HashMap, num::NonZeroUsize, path::Path};

mod rst;
mod scoper;
pub use rst::*;
use rustc_hash::FxHashMap;

use crate::{
    parser,
    string_storage::StringStorage,
    tokenizer::{Intern, Span},
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
}

type Result<'s, T> = std::result::Result<T, ReifyError<'s>>;

pub fn reify<'s>(storage: &'s StringStorage, expr: parser::Expr<'s>) -> Result<'s, Module<'s>> {
    Reifier {
        scoper: Scoper::default(),
        sym_counter: NonZeroUsize::new(1).unwrap(),
        module: Module::default(),
    }
    .reify(expr)
}

struct Reifier<'s> {
    sym_counter: NonZeroUsize,
    scoper: Scoper<'s>,
    module: Module<'s>,
}

impl<'s> Reifier<'s> {
    fn reify(mut self, expr: parser::Expr<'s>) -> Result<'s, Module<'s>> {
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

        self.scope(scope)?;

        Ok(self.module)
    }

    fn scope(&mut self, scope: parser::Scope<'s>) -> Result<'s, Scope<'s>> {
        self.scoper.push();

        for def in scope.defs.into_vec() {
            match def.value.kind {
                parser::ExprKind::Abstract {
                    arg,
                    spec,
                    ty,
                    body,
                } => self.def(def.name, spec, arg, body, ty)?,
                _ => self.typedef(def.name, def.value)?,
            }
        }

        let mut exprs = Vec::new();
        for expr in scope.exprs.into_vec() {}

        self.scoper.pop();

        Ok(Scope {
            exprs: exprs.into_boxed_slice(),
            discard: scope.discard,
        })
    }

    fn def(
        &mut self,
        name: Intern<'s>,
        spec: bool,
        arg: Option<Box<parser::Expr<'s>>>,
        body: Box<parser::Expr<'s>>,
        ty: Option<Box<parser::Expr<'s>>>,
    ) -> Result<'s, ()> {
        let sym = self.scoper.new_symbol(name);

        let body = if let parser::ExprKind::Scope(scope) = body.kind {
            Expr {
                kind: ExprKind::Scope(self.scope(scope)?),
                span: body.span,
                ty: if let Some(ty) = ty {
                    Some(self.type_(*ty)?)
                } else {
                    None
                },
            }
        } else {
            self.expr(*body)?
        };

        let arg = if let Some(arg) = arg {
            Some(self.pattern(*arg)?)
        } else {
            None
        };
        self.module.defs.insert(
            sym,
            Proc {
                spec,
                name,
                arg,
                body,
            },
        );

        Ok(())
    }

    fn typedef(&mut self, name: Intern<'s>, value: Box<parser::Expr<'s>>) -> Result<'s, ()> {
        let sym = self.scoper.new_symbol(name);
        let type_ = self.type_(*value)?;
        self.module
            .types
            .insert(sym, TypeDef { name, inner: type_ });
        Ok(())
    }

    fn type_(&mut self, expr: parser::Expr<'s>) -> Result<'s, Type<'s>> {
        todo!()
    }

    fn pattern(&mut self, expr: parser::Expr<'s>) -> Result<'s, Pattern<'s>> {
        todo!()
    }

    fn expr(&mut self, expr: parser::Expr<'s>) -> Result<'s, Expr<'s>> {
        let mut type_ = None;
        let kind = match expr.kind {
            parser::ExprKind::Scope(scope) => ExprKind::Scope(self.scope(scope)?),
            parser::ExprKind::Abstract {
                spec,
                arg,
                body,
                ty,
            } => {
                if let Some(ty) = ty {
                    type_ = Some(self.type_(*ty)?);
                }

                ExprKind::Abstract {
                    spec,
                    arg: if let Some(arg) = arg {
                        Some(self.pattern(*arg)?)
                    } else {
                        None
                    },
                    body: Box::new(self.expr(*body)?),
                }
            }
            parser::ExprKind::For {
                init,
                cond,
                afterthought,
                body,
            } => ExprKind::For {
                init: if let Some(init) = init {
                    Some(Box::new(self.expr(*init)?))
                } else {
                    None
                },
                cond: Box::new(self.expr(*cond)?),
                afterthought: if let Some(afterthought) = afterthought {
                    Some(Box::new(self.expr(*afterthought)?))
                } else {
                    None
                },
                body: Box::new(self.expr(*body)?),
            },
            parser::ExprKind::Case {
                cond,
                on_true,
                on_false,
            } => ExprKind::Case {
                cond: Box::new(self.expr(*cond)?),
                on_true: Box::new(self.expr(*on_true)?),
                on_false: if let Some(on_false) = on_false {
                    Some(Box::new(self.expr(*on_false)?))
                } else {
                    None
                },
            },
            parser::ExprKind::Tuple { items } => ExprKind::Tuple {
                items: {
                    let mut reified_items = Vec::with_capacity(items.len());
                    for item in items.into_vec() {
                        reified_items.push(self.expr(item)?)
                    }
                    reified_items.into_boxed_slice()
                },
            },
            parser::ExprKind::Assert { expr, ty } => {
                type_ = Some(self.type_(*ty)?);
                self.expr(*expr)?.kind
            }
            parser::ExprKind::Binary(op, a, b) => match op {
                BinOp::Eq => {
                    let ahs = Self::has_solve(&a)?;
                    let bhs = Self::has_solve(&b)?;
                    if ahs && bhs {
                        return Err(ReifyError {
                            kind: ReifyErrorKind::ImpossibleSolve,
                            span: Some(expr.span),
                        });
                    } else if ahs {
                        let pat = self.pattern(*a)?;
                        let val = self.expr(*b)?;
                        ExprKind::StructuralEq(Box::new(pat), Box::new(val))
                    } else if bhs {
                        let pat = self.pattern(*b)?;
                        let val = self.expr(*a)?;
                        ExprKind::StructuralEq(Box::new(pat), Box::new(val))
                    } else {
                        ExprKind::Binary(op, Box::new(self.expr(*a)?), Box::new(self.expr(*b)?))
                    }
                }
                _ => ExprKind::Binary(op, Box::new(self.expr(*a)?), Box::new(self.expr(*b)?)),
            },
            parser::ExprKind::Unary(op, a) => ExprKind::Unary(op, Box::new(self.expr(*a)?)),
            parser::ExprKind::Apply(a, b) => {
                ExprKind::Apply(Box::new(self.expr(*a)?), Box::new(self.expr(*b)?))
            }
            parser::ExprKind::Solve(marker, _) => {
                return Err(ReifyError {
                    kind: ReifyErrorKind::UnexpectedMarker(marker),
                    span: Some(expr.span),
                })
            }
            parser::ExprKind::Name(name) => ExprKind::Symbol({
                if let Some(symbol) = self.scoper.lookup(name) {
                    symbol
                } else {
                    return Err(ReifyError {
                        kind: ReifyErrorKind::UndefinedSymbol(name),
                        span: Some(expr.span),
                    });
                }
            }),
            parser::ExprKind::Literal(lit) => ExprKind::Literal(lit),
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
            parser::ExprKind::Assert { expr, .. } => Self::has_solve(&expr)?,
            parser::ExprKind::Apply(a, b) => Self::has_solve(&a)? || Self::has_solve(&b)?,
            parser::ExprKind::Solve(_, _) => true,
            _ => false,
        })
    }
}
