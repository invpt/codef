use crate::{
    char_reader::CharReader,
    errors::ErrorStream,
    tokenizer::{Span, Token, TokenKind, TokenizationError, Tokens},
};

mod ast;
mod preds;
pub mod utils;

pub use ast::*;
use preds::*;

#[derive(Debug)]
pub struct ParseError<'s> {
    pub kind: ParseErrorKind<'s>,
    pub span: Option<Span>,
}

#[derive(Debug)]
pub enum ParseErrorKind<'s> {
    Unexpected(Option<Token<'s>>),
    TokenizationError(TokenizationError),
}

impl<'s> From<TokenizationError> for ParseError<'s> {
    fn from(err: TokenizationError) -> Self {
        ParseError {
            span: err.span,
            kind: ParseErrorKind::TokenizationError(err),
        }
    }
}

type Result<'s, T> = std::result::Result<T, ParseError<'s>>;

pub fn parse<'s>(
    tokens: Tokens<'s, impl CharReader>,
    errors: &'s ErrorStream<'s>,
) -> Result<'s, Expr<'s>> {
    Parser { tokens, errors }.parse()
}

struct Parser<'s, R> {
    tokens: Tokens<'s, R>,
    errors: &'s ErrorStream<'s>,
}

impl<'s, R: CharReader> Parser<'s, R> {
    fn parse(mut self) -> Result<'s, Expr<'s>> {
        let scope = self.scope(vpred!())?;

        let (kind, span) = match scope {
            ParsedScope::Scope(scope) => {
                let mut spans = [
                    scope.defs.first().map(|d| d.span.start),
                    scope.body.first().map(|e| e.span.start),
                    scope.defs.last().map(|d| d.span.end),
                    scope.body.last().map(|e| e.span.end),
                ]
                .into_iter()
                .filter_map(std::convert::identity);

                (
                    ExprKind::Object(Box::new(scope)),
                    Span {
                        start: spans.next().unwrap_or(0),
                        end: spans.last().unwrap_or(0),
                    },
                )
            }
            ParsedScope::Expr { kind, span } => (kind, span.unwrap_or(Span { start: 0, end: 0 })),
        };

        Ok(Expr { kind, span })
    }

    fn def(&mut self) -> Result<'s, Def<'s>> {
        let Token {
            span: Span { start, .. },
            ..
        } = self.require(tpred!(TokenKind::Def))?;
        let name = self.require(vpred!(TokenKind::Name(n) => n))?;
        let (value, needs_semi) = self.block_needs_semi()?;
        let end = if let NeedsSemi::Yes = needs_semi {
            self.require(vpred!(:t: TokenKind::Semicolon => t.span.end))?
        } else {
            value.span.end
        };

        Ok(Def {
            name,
            value: Box::new(value),
            span: Span { start, end },
        })
    }

    fn block(&mut self) -> Result<'s, Expr<'s>> {
        Ok(self.block_needs_semi()?.0)
    }

    fn block_needs_semi(&mut self) -> Result<'s, (Expr<'s>, NeedsSemi)> {
        if let Some(open) = self.eat(tpred!(TokenKind::OpenBrace))? {
            let scope = self.scope(bpred!(TokenKind::CloseBrace))?;
            let close = self.require(tpred!(TokenKind::CloseBrace))?;
            Ok((
                Expr {
                    span: Span {
                        start: open.span.start,
                        end: close.span.end,
                    },
                    kind: match scope {
                        ParsedScope::Scope(scope) => ExprKind::Block(Box::new(scope)),
                        ParsedScope::Expr { kind, .. } => kind,
                    },
                },
                NeedsSemi::No,
            ))
        } else if let Some(open) = self.eat(tpred!(TokenKind::DotOpenBrace))? {
            let scope = self.scope(bpred!(TokenKind::CloseBrace))?;
            let close = self.require(tpred!(TokenKind::CloseBrace))?;
            Ok((
                Expr {
                    span: Span {
                        start: open.span.start,
                        end: close.span.end,
                    },
                    kind: match scope {
                        ParsedScope::Scope(scope) => ExprKind::Object(Box::new(scope)),
                        ParsedScope::Expr { kind, .. } => kind, // TODO: return Err here
                    },
                },
                NeedsSemi::No,
            ))
        } else {
            Ok(self.lambda_needs_semi()?)
        }
    }

    fn scope(
        &mut self,
        end_pred: impl Fn(&Token<'s>) -> Option<()>,
    ) -> Result<'s, ParsedScope<'s>> {
        if self.has_peek(&end_pred)? {
            return Ok(ParsedScope::Expr {
                kind: ExprKind::Tuple {
                    items: Box::new([]),
                },
                span: None,
            });
        }

        let mut defs;
        let mut body;
        if !self.has_peek(bpred!(TokenKind::Def))? {
            let first = self.tuple()?;

            if self.has_peek(&end_pred)? {
                return Ok(ParsedScope::Expr {
                    kind: first.kind,
                    span: Some(first.span),
                });
            } else {
                defs = vec![];
                body = vec![first];

                self.require(bpred!(TokenKind::Semicolon))?;
            }
        } else {
            defs = Vec::new();
            body = Vec::new();
        }

        let mut semi = true;
        while let Some(None) = self.tokens.peek()?.map(&end_pred) {
            if self.has_peek(to_bpred(&end_pred))? {
                break;
            } else if self.has_peek(bpred!(TokenKind::Def))? {
                defs.push(self.def()?)
            } else {
                let expr = self.tuple()?;
                body.push(expr);

                if self.eat(bpred!(TokenKind::Semicolon))?.is_none() {
                    semi = false;
                    break;
                } else {
                    semi = true;
                }
            }
        }

        Ok(ParsedScope::Scope(Scope {
            defs: defs.into(),
            body: body.into(),
            trailing_semi: semi,
        }))
    }

    fn tuple(&mut self) -> Result<'s, Expr<'s>> {
        Ok(self.tuple_needs_semi()?.0)
    }

    fn tuple_needs_semi(&mut self) -> Result<'s, (Expr<'s>, NeedsSemi)> {
        let first = self.block_needs_semi()?;

        if self.has_peek(bpred!(TokenKind::Comma))? {
            let mut items = vec![first.0];

            while self.eat(bpred!(TokenKind::Comma))?.is_some() {
                let item = self.block()?;

                items.push(item);
            }

            let span = Span {
                start: items.first().unwrap().span.start,
                end: items.last().unwrap().span.end,
            };

            Ok((
                Expr {
                    kind: ExprKind::Tuple {
                        items: items.into(),
                    },
                    span,
                },
                NeedsSemi::Yes,
            ))
        } else {
            Ok(first)
        }
    }

    fn lambda_needs_semi(&mut self) -> Result<'s, (Expr<'s>, NeedsSemi)> {
        let mut a = self.expr_needs_semi()?;

        if let Some(open) = self.eat(tpred!(TokenKind::OpenBrace))? {
            let scope = self.scope(bpred!(TokenKind::CloseBrace))?;
            let close = self.require(tpred!(TokenKind::CloseBrace))?;

            a = (
                Expr {
                    span: Span {
                        start: a.0.span.start,
                        end: close.span.end,
                    },
                    kind: ExprKind::Lambda {
                        arg: Box::new(a.0),
                        body: Box::new(Expr {
                            kind: match scope {
                                ParsedScope::Scope(scope) => ExprKind::Block(Box::new(scope)),
                                ParsedScope::Expr { kind, .. } => kind,
                            },
                            span: Span {
                                start: open.span.start,
                                end: close.span.end,
                            },
                        }),
                    },
                },
                NeedsSemi::No,
            );
        } else if let Some(open) = self.eat(tpred!(TokenKind::DotOpenBrace))? {
            let scope = self.scope(bpred!(TokenKind::CloseBrace))?;
            let close = self.require(tpred!(TokenKind::CloseBrace))?;

            a = (
                Expr {
                    span: Span {
                        start: a.0.span.start,
                        end: close.span.end,
                    },
                    kind: ExprKind::Lambda {
                        arg: Box::new(a.0),
                        body: Box::new(Expr {
                            kind: match scope {
                                ParsedScope::Scope(scope) => ExprKind::Block(Box::new(scope)),
                                ParsedScope::Expr { kind, .. } => kind, // TODO: return Err here
                            },
                            span: Span {
                                start: open.span.start,
                                end: close.span.end,
                            },
                        }),
                    },
                },
                NeedsSemi::No,
            );
        }

        Ok(a)
    }

    fn expr(&mut self) -> Result<'s, Expr<'s>> {
        Ok(self.expr_needs_semi()?.0)
    }

    fn expr_needs_semi(&mut self) -> Result<'s, (Expr<'s>, NeedsSemi)> {
        if self.has_peek(bpred!(TokenKind::Case))? {
            return Ok((self.case()?, NeedsSemi::No));
        }

        let mut a = (self.logical()?, NeedsSemi::Yes);

        if self.eat(bpred!(TokenKind::ColonColon))?.is_some() {
            let b = self.logical()?;

            a = (
                Expr {
                    span: Span {
                        start: a.0.span.start,
                        end: b.span.end,
                    },
                    kind: ExprKind::TypeAssertion {
                        a: Box::new(a.0),
                        b: Box::new(b),
                    },
                },
                NeedsSemi::Yes,
            );
        }

        Ok(a)
    }

    fn case(&mut self) -> Result<'s, Expr<'s>> {
        let case = self.require(tpred!(TokenKind::Case))?;

        self.case_inner(case.span.start)
    }

    fn case_inner(&mut self, start: usize) -> Result<'s, Expr<'s>> {
        let cond = self.expr()?;
        let on_true_open = self.require(tpred!(TokenKind::OpenBrace))?;
        let on_true = self.scope(bpred!(TokenKind::CloseBrace))?;
        let on_true_close = self.require(tpred!(TokenKind::CloseBrace))?;
        let on_true = Expr {
            span: Span {
                start: on_true_open.span.start,
                end: on_true_close.span.end,
            },
            kind: match on_true {
                ParsedScope::Scope(scope) => ExprKind::Block(Box::new(scope)),
                ParsedScope::Expr { kind, .. } => kind,
            },
        };
        if let Some(r#else) = self.eat(tpred!(TokenKind::Else))? {
            if let Some(on_false_open) = self.eat(tpred!(TokenKind::OpenBrace))? {
                let on_false = self.scope(bpred!(TokenKind::CloseBrace))?;
                let on_false_close = self.require(tpred!(TokenKind::CloseBrace))?;
                let on_false = Expr {
                    span: Span {
                        start: on_false_open.span.start,
                        end: on_false_close.span.end,
                    },
                    kind: match on_false {
                        ParsedScope::Scope(scope) => ExprKind::Block(Box::new(scope)),
                        ParsedScope::Expr { kind, .. } => kind,
                    },
                };

                Ok(Expr {
                    span: Span {
                        start,
                        end: on_false.span.end,
                    },
                    kind: ExprKind::Branch {
                        cond: Box::new(cond),
                        on_true: Box::new(on_true),
                        on_false: Some(Box::new(on_false)),
                    },
                })
            } else {
                let inner = self.case_inner(r#else.span.start)?;

                Ok(Expr {
                    span: Span {
                        start,
                        end: inner.span.end,
                    },
                    kind: ExprKind::Branch {
                        cond: Box::new(cond),
                        on_true: Box::new(on_true),
                        on_false: Some(Box::new(inner)),
                    },
                })
            }
        } else {
            Ok(Expr {
                span: Span {
                    start,
                    end: on_true.span.end,
                },
                kind: ExprKind::Branch {
                    cond: Box::new(cond),
                    on_true: Box::new(on_true),
                    on_false: None,
                },
            })
        }
    }

    fn logical(&mut self) -> Result<'s, Expr<'s>> {
        self.bin_op(
            Self::equal,
            vpred! {
                TokenKind::AmpAmp => BinOp::And,
                TokenKind::PipePipe => BinOp::Or,
            },
        )
    }

    fn equal(&mut self) -> Result<'s, Expr<'s>> {
        self.bin_op(
            Self::cmp,
            vpred! {
                TokenKind::Equal => BinOp::Equal,
                TokenKind::NotEqual => BinOp::NotEqual,
            },
        )
    }

    fn cmp(&mut self) -> Result<'s, Expr<'s>> {
        self.bin_op(
            Self::terms,
            vpred! {
                TokenKind::Gt => BinOp::Gt,
                TokenKind::GtEq => BinOp::GtEq,
                TokenKind::Lt => BinOp::Lt,
                TokenKind::LtEq => BinOp::LtEq,
            },
        )
    }

    fn terms(&mut self) -> Result<'s, Expr<'s>> {
        self.bin_op(
            Self::factors,
            vpred! {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
            },
        )
    }

    fn factors(&mut self) -> Result<'s, Expr<'s>> {
        self.bin_op(
            Self::prefix,
            vpred! {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                TokenKind::Percent => BinOp::Mod,
            },
        )
    }

    fn prefix(&mut self) -> Result<'s, Expr<'s>> {
        if let Some((op_span, op)) = self.eat(vpred! {
            :t: TokenKind::Bang => (t.span, UnOp::Not),
            :t: TokenKind::Set => (t.span, UnOp::Set),
            :t: TokenKind::Val => (t.span, UnOp::Val),
            :t: TokenKind::Caret => (t.span, UnOp::Ref),
        })? {
            let a = self.prefix()?;

            let span = Span {
                start: op_span.start,
                end: a.span.end,
            };

            Ok(Expr {
                kind: ExprKind::UnOp {
                    op,
                    arg: Box::new(a),
                },
                span,
            })
        } else {
            self.suffix()
        }
    }

    fn suffix(&mut self) -> Result<'s, Expr<'s>> {
        let Some(mut a) = self.maybe_atom()? else {
            return Err(ParseError {
                kind: ParseErrorKind::Unexpected(self.tokens.peek()?.cloned()),
                span: None,
            })
        };

        loop {
            if let Some(caret) = self.eat(tpred!(TokenKind::Caret))? {
                let span = Span {
                    start: a.span.start,
                    end: caret.span.end,
                };

                a = Expr {
                    kind: ExprKind::UnOp {
                        op: UnOp::Deref,
                        arg: Box::new(a),
                    },
                    span,
                }
            } else if self.eat(tpred!(TokenKind::Dot))?.is_some() {
                let (prop_span, prop) =
                    self.require(vpred!(:t: TokenKind::Name(n) => (t.span, n)))?;

                let span = Span {
                    start: a.span.start,
                    end: prop_span.end,
                };

                a = Expr {
                    kind: ExprKind::Access {
                        expr: Box::new(a),
                        prop: AccessRhs::Prop(prop),
                    },
                    span,
                }
            } else if let Some(arg) = self.maybe_atom()? {
                a = Expr {
                    span: Span {
                        start: a.span.start,
                        end: arg.span.end,
                    },
                    kind: ExprKind::Apply {
                        a: Box::new(a),
                        b: Box::new(arg),
                    },
                }
            } else {
                break;
            }
        }

        Ok(a)
    }

    fn maybe_atom(&mut self) -> Result<'s, Option<Expr<'s>>> {
        if let Some(open) = self.eat(tpred!(TokenKind::OpenParen))? {
            let scope = self.scope(bpred!(TokenKind::CloseParen))?;
            let close = self.require(tpred!(TokenKind::CloseParen))?;
            Ok(Some(Expr {
                span: Span {
                    start: open.span.start,
                    end: close.span.end,
                },
                kind: match scope {
                    ParsedScope::Scope(scope) => ExprKind::Block(Box::new(scope)),
                    ParsedScope::Expr { kind, .. } => kind,
                },
            }))
        } else if self.has_peek(bpred!(TokenKind::Pipe))? {
            Ok(Some(self.variant()?))
        } else if let Some((span, kind)) = self.eat(vpred! {
            :t: TokenKind::Float(f) => (t.span, ExprKind::Literal(Literal::Float(f))),
            :t: TokenKind::Integer(i) => (t.span, ExprKind::Literal(Literal::Integer(i))),
            :t: TokenKind::String(s) => (t.span, ExprKind::Literal(Literal::String(s))),
            :t: TokenKind::Name(n) => (t.span, ExprKind::Ident(n)),
        })? {
            Ok(Some(Expr { span, kind }))
        } else {
            Ok(None)
        }
    }

    fn variant(&mut self) -> Result<'s, Expr<'s>> {
        let mut items = Vec::with_capacity(1);
        while let Some(pipe) = self.eat(tpred!(TokenKind::Pipe))? {
            let start = pipe.span.start;
            let (name_span, name) = self.require(vpred!(:t: TokenKind::Name(n) => (t.span, n)))?;
            let value;
            let end;
            if self.eat(bpred!(TokenKind::Colon))?.is_some() {
                let expr = self.expr()?;
                end = expr.span.end;
                value = Some(expr);
            } else {
                end = name_span.end;
                value = None;
            }

            items.push(VariantItem {
                name,
                value,
                span: Span { start, end },
            })
        }

        Ok(Expr {
            span: Span {
                start: items.first().unwrap().span.start,
                end: items.last().unwrap().span.end,
            },
            kind: ExprKind::Variant(items.into()),
        })
    }

    fn bin_op(
        &mut self,
        next: impl Fn(&mut Self) -> Result<'s, Expr<'s>>,
        pred: impl Fn(&Token<'s>) -> Option<BinOp>,
    ) -> Result<'s, Expr<'s>> {
        let mut a = next(self)?;

        while let Some(op) = self.eat(&pred)? {
            let b = next(self)?;

            let span = Span {
                start: a.span.start,
                end: a.span.end,
            };

            a = Expr {
                kind: ExprKind::BinOp {
                    op,
                    lhs: Box::new(a),
                    rhs: Box::new(b),
                },
                span,
            }
        }

        Ok(a)
    }

    /// Returns `true` if the current token peek satisfies `pred`.
    fn has_peek(&mut self, pred: impl Fn(&Token<'s>) -> Option<()>) -> Result<'s, bool> {
        if let Some(token) = self.tokens.peek()? {
            if pred(token).is_some() {
                Ok(true)
            } else {
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    /// Requires that the next token exists and satisfies `pred` and errors otherwise.
    ///
    /// Does not consume the token if it does not satisfy `pred`.
    fn require<T>(&mut self, pred: impl Fn(&Token<'s>) -> Option<T>) -> Result<'s, T> {
        match self.maybe_require(pred) {
            Ok(Some(t)) => Ok(t),
            Ok(None) => Err(ParseError {
                kind: ParseErrorKind::Unexpected(None),
                span: None,
            }),
            Err(e) => Err(e),
        }
    }

    /// Requires that the next token (if one exists) satisfies `pred` and errors otherwise.
    ///
    /// Does not consume the token if it does not satisfy `pred`.
    fn maybe_require<T>(
        &mut self,
        pred: impl Fn(&Token<'s>) -> Option<T>,
    ) -> Result<'s, Option<T>> {
        if let Some(token) = self.tokens.peek()? {
            if let Some(t) = pred(token) {
                self.tokens.next()?;
                Ok(Some(t))
            } else {
                println!("{}", std::backtrace::Backtrace::capture());
                Err(ParseError {
                    span: Some(token.span),
                    kind: ParseErrorKind::Unexpected(Some(token.clone())),
                })
            }
        } else {
            Ok(None)
        }
    }

    /// Checks if the next token (if one exists) satisfies `pred` and returns None otherwise.
    ///
    /// Does not consume the token if it does not satisfy `pred`.
    fn eat<T>(&mut self, pred: impl Fn(&Token<'s>) -> Option<T>) -> Result<'s, Option<T>> {
        if let Some(token) = self.tokens.peek()? {
            if let Some(t) = pred(token) {
                self.tokens.next()?;
                Ok(Some(t))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
    }
}

enum NeedsSemi {
    Yes,
    No,
}

enum ParsedScope<'s> {
    Scope(Scope<'s>),
    Expr {
        kind: ExprKind<'s>,
        span: Option<Span>,
    },
}
