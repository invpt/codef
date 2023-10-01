use crate::{
    char_reader::CharReader,
    errors::ErrorStream,
    tokenizer::{Span, Token, TokenKind, TokenizationError, Tokens}, string_storage::StringInterner,
};

mod ast;
mod preds;

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

pub fn parse<'i, 's>(
    tokens: Tokens<'i, 's, impl CharReader>,
    errors: &'s ErrorStream<'s>,
) -> Result<'s, (&'i mut StringInterner<'s>, Expr<'s>)> {
    let mut parser = Parser { tokens, errors };
    let res = parser.parse()?;
    Ok((parser.tokens.strings, res))    
}

struct Parser<'i, 's, R> {
    tokens: Tokens<'i, 's, R>,
    errors: &'s ErrorStream<'s>,
}

impl<'i, 's, R: CharReader> Parser<'i, 's, R> {
    fn parse(&mut self) -> Result<'s, Expr<'s>> {
        self.scope(bpred!())
    }

    fn scope(&mut self, end_pred: impl Fn(&Token<'s>) -> Option<()>) -> Result<'s, Expr<'s>> {
        let mut start = 0;
        let mut end = 0;
        let mut defs = Vec::new();
        let mut exprs = Vec::with_capacity(1);
        let mut first = true;
        let mut discard = false;
        while self.tokens.peek()?.is_some() && !self.has_peek(&end_pred)? {
            let mut stop = false;
            let span = if self.has_peek(bpred!(TokenKind::Def))? {
                let def = self.def()?;
                let span = def.decl_span;
                defs.push(def);
                discard = true;
                span
            } else if self.has_peek(bpred!(TokenKind::Type))? {
                let def = self.typedef()?;
                let span = def.decl_span;
                defs.push(def);
                discard = true;
                span
            } else if self.has_peek(bpred!(TokenKind::Case))? {
                let case = self.termcase()?;
                let span = case.span;
                exprs.push(case);
                discard = false;
                span
            } else if self.has_peek(bpred!(TokenKind::For))? {
                let for_ = self.termfor()?;
                let span = for_.span;
                exprs.push(for_);
                discard = true;
                span
            } else {
                let expr = self.tuple(mergepreds(bpred!(TokenKind::Semicolon), &end_pred))?;
                let span = expr.span;
                exprs.push(expr);
                let semi = self.eat(tpred!(TokenKind::Semicolon))?;
                discard = semi.is_some();
                stop = semi.is_none();
                Span {
                    start: span.start,
                    end: semi.map(|s| s.span.end).unwrap_or(span.end),
                }
            };
            if first {
                start = span.start;
            }
            end = span.end;
            first = false;

            if stop {
                break;
            }
        }

        if defs.is_empty() && exprs.len() == 1 && !discard {
            Ok(exprs.pop().unwrap())
        } else if defs.is_empty() && exprs.is_empty() {
            Ok(Expr {
                span: Span { start, end },
                kind: ExprKind::Tuple {
                    items: Box::new([]),
                },
            })
        } else {
            Ok(Expr {
                span: Span { start, end },
                kind: ExprKind::Scope(Scope {
                    defs: defs.into_boxed_slice(),
                    exprs: exprs.into_boxed_slice(),
                    discard,
                }),
            })
        }
    }

    fn def(&mut self) -> Result<'s, Def<'s>> {
        let kw_tok = self.require(tpred!(TokenKind::Def))?;
        let (name_span, name) = self.require(vpred!(:t: TokenKind::Name(n) => (t.span, n)))?;
        let abs = self.termexpr()?;

        Ok(Def {
            decl_span: Span {
                start: kw_tok.span.start,
                end: name_span.end,
            },
            name,
            value: Box::new(abs),
        })
    }

    fn typedef(&mut self) -> Result<'s, Def<'s>> {
        let kw_tok = self.require(tpred!(TokenKind::Type))?;
        let (name_span, name) = self.require(vpred!(:t: TokenKind::Name(n) => (t.span, n)))?;
        let value = self.termexpr()?;

        Ok(Def {
            decl_span: Span {
                start: kw_tok.span.start,
                end: name_span.end,
            },
            name,
            value: Box::new(value),
        })
    }

    fn termexpr(&mut self) -> Result<'s, Expr<'s>> {
        if self.has_peek(bpred!(
            TokenKind::Dollar | TokenKind::ThinArrow | TokenKind::FatArrow | TokenKind::OpenBrace
        ))? {
            let ty = if self.eat(bpred!(TokenKind::ThinArrow))?.is_some() {
                Some(self.logical()?)
            } else {
                None
            };
            let spec = self.eat(bpred!(TokenKind::Dollar))?.is_some();
            let body = self.termbody()?;

            Ok(Expr {
                span: Span {
                    start: body.span.start,
                    end: body.span.end,
                },
                kind: ExprKind::Abstract {
                    arg: None,
                    spec,
                    ty: ty.map(Box::new),
                    body: Box::new(body),
                },
            })
        } else {
            let logical = self.logical()?;

            if self.has_peek(bpred!(
                TokenKind::Dollar
                    | TokenKind::ThinArrow
                    | TokenKind::FatArrow
                    | TokenKind::OpenBrace
            ))? {
                let ty = if self.eat(bpred!(TokenKind::ThinArrow))?.is_some() {
                    Some(self.logical()?)
                } else {
                    None
                };
                let spec = self.eat(bpred!(TokenKind::Dollar))?.is_some();
                let body = self.termbody()?;

                Ok(Expr {
                    span: Span {
                        start: logical.span.start,
                        end: body.span.end,
                    },
                    kind: ExprKind::Abstract {
                        arg: Some(Box::new(logical)),
                        spec,
                        ty: ty.map(Box::new),
                        body: Box::new(body),
                    },
                })
            } else {
                self.require(bpred!(TokenKind::Semicolon))?;
                Ok(logical)
            }
        }
    }

    fn termbody(&mut self) -> Result<'s, Expr<'s>> {
        if self.eat(bpred!(TokenKind::FatArrow))?.is_some() {
            let body = self.termexpr()?;
            Ok(body)
        } else {
            let open = self.require(tpred!(TokenKind::OpenBrace))?;
            let scope = self.scope(bpred!(TokenKind::CloseBrace))?;
            let close = self.require(tpred!(TokenKind::CloseBrace))?;

            Ok(Expr {
                kind: scope.kind,
                span: Span {
                    start: open.span.start,
                    end: close.span.end,
                },
            })
        }
    }

    fn termcase(&mut self) -> Result<'s, Expr<'s>> {
        let case_tok = self.require(tpred!(TokenKind::Case))?;
        let cond = self.logical()?;
        let on_true = self.termbody()?;
        let on_false = self.termelse()?;

        Ok(Expr {
            span: Span {
                start: case_tok.span.start,
                end: on_false
                    .as_ref()
                    .map(|f| f.span.end)
                    .unwrap_or(on_true.span.end),
            },
            kind: ExprKind::Case {
                cond: Box::new(cond),
                on_true: Box::new(on_true),
                on_false: on_false.map(Box::new),
            },
        })
    }

    fn termelse(&mut self) -> Result<'s, Option<Expr<'s>>> {
        let Some(else_tok) = self.eat(tpred!(TokenKind::Else))? else {
            return Ok(None)
        };

        if self.has_peek(bpred!(TokenKind::FatArrow | TokenKind::OpenBrace))? {
            return Ok(Some(self.termbody()?));
        }

        let cond = self.logical()?;
        let on_true = self.termbody()?;
        let on_false = self.termelse()?;

        Ok(Some(Expr {
            span: Span {
                start: else_tok.span.start,
                end: on_false
                    .as_ref()
                    .map(|f| f.span.end)
                    .unwrap_or(on_true.span.end),
            },
            kind: ExprKind::Case {
                cond: Box::new(cond),
                on_true: Box::new(on_true),
                on_false: on_false.map(Box::new),
            },
        }))
    }

    fn termfor(&mut self) -> Result<'s, Expr<'s>> {
        let for_tok = self.require(tpred!(TokenKind::For))?;
        let first = self.logical()?;
        let mut second = None;
        let mut third = None;
        if self.eat(bpred!(TokenKind::Semicolon))?.is_some() {
            second = Some(self.logical()?);
            if self.eat(bpred!(TokenKind::Semicolon))?.is_some() {
                third = Some(self.logical()?);
            }
        }
        let body = self.termbody()?;

        Ok(Expr {
            span: Span {
                start: for_tok.span.start,
                end: body.span.end,
            },
            kind: {
                if let Some(second) = second {
                    if let Some(third) = third {
                        ExprKind::For {
                            init: Some(Box::new(first)),
                            cond: Box::new(second),
                            afterthought: Some(Box::new(third)),
                            body: Box::new(body),
                        }
                    } else {
                        ExprKind::For {
                            init: Some(Box::new(first)),
                            cond: Box::new(second),
                            afterthought: None,
                            body: Box::new(body),
                        }
                    }
                } else {
                    ExprKind::For {
                        init: None,
                        cond: Box::new(first),
                        afterthought: None,
                        body: Box::new(body),
                    }
                }
            },
        })
    }

    fn tuple(&mut self, end_pred: impl Fn(&Token<'s>) -> Option<()>) -> Result<'s, Expr<'s>> {
        if self.has_peek(&end_pred)? || self.tokens.peek()?.is_none() {
            return Ok(Expr {
                kind: ExprKind::Tuple {
                    items: Box::new([]),
                },
                // TODO: proper span here
                span: Span { start: 0, end: 0 },
            });
        }

        let first = self.expr()?;

        if self.has_peek(&end_pred)? {
            return Ok(first);
        }

        let start = first.span.start;
        let mut end = first.span.end;
        let mut items = Vec::from([first]);
        loop {
            let Some(comma_tok) = self.eat(tpred!(TokenKind::Comma))? else { break };
            end = comma_tok.span.end;
            if self.has_peek(&end_pred)? {
                break;
            }

            items.push(self.expr()?);
        }

        Ok(Expr {
            kind: ExprKind::Tuple {
                items: items.into_boxed_slice(),
            },
            span: Span { start, end },
        })
    }

    fn expr(&mut self) -> Result<'s, Expr<'s>> {
        let logical = self.logical()?;

        if self.has_peek(bpred!(
            TokenKind::Dollar | TokenKind::ThinArrow | TokenKind::FatArrow | TokenKind::OpenBrace
        ))? {
            let ty = if self.eat(bpred!(TokenKind::ThinArrow))?.is_some() {
                Some(self.logical()?)
            } else {
                None
            };
            let spec = self.eat(bpred!(TokenKind::Dollar))?.is_some();
            let body = self.body()?;

            Ok(Expr {
                span: Span {
                    start: logical.span.start,
                    end: body.span.end,
                },
                kind: ExprKind::Abstract {
                    arg: Some(Box::new(logical)),
                    spec,
                    ty: ty.map(Box::new),
                    body: Box::new(body),
                },
            })
        } else {
            Ok(logical)
        }
    }

    fn body(&mut self) -> Result<'s, Expr<'s>> {
        if self.eat(bpred!(TokenKind::FatArrow))?.is_some() {
            let body = self.logical()?;
            Ok(body)
        } else {
            let open = self.require(tpred!(TokenKind::OpenBrace))?;
            let scope = self.scope(bpred!(TokenKind::CloseBrace))?;
            let close = self.require(tpred!(TokenKind::CloseBrace))?;

            Ok(Expr {
                kind: scope.kind,
                span: Span {
                    start: open.span.start,
                    end: close.span.end,
                },
            })
        }
    }

    fn logical(&mut self) -> Result<'s, Expr<'s>> {
        self.bin_op(
            Self::cmp,
            vpred! {
                TokenKind::AmpAmp => BinOp::And,
                TokenKind::PipePipe => BinOp::Or,
            },
        )
    }

    fn cmp(&mut self) -> Result<'s, Expr<'s>> {
        self.bin_op(
            Self::assert,
            vpred! {
                TokenKind::Equal => BinOp::Eq,
                TokenKind::NotEqual => BinOp::Neq,
                TokenKind::Gt => BinOp::Gt,
                TokenKind::GtEq => BinOp::Geq,
                TokenKind::Lt => BinOp::Lt,
                TokenKind::LtEq => BinOp::Leq,
            },
        )
    }

    fn assert(&mut self) -> Result<'s, Expr<'s>> {
        let expr = self.arith()?;
        if let Some(tok) = self.eat(tpred!(TokenKind::ColonColon))? {
            let ty = self.arith()?;
            Ok(Expr {
                span: Span {
                    start: expr.span.start,
                    end: tok.span.end,
                },
                kind: ExprKind::Assert {
                    expr: Box::new(expr),
                    ty: Box::new(ty),
                },
            })
        } else {
            Ok(expr)
        }
    }

    fn arith(&mut self) -> Result<'s, Expr<'s>> {
        self.bin_op(
            Self::term,
            vpred! {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
            },
        )
    }

    fn term(&mut self) -> Result<'s, Expr<'s>> {
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
            :t: TokenKind::Minus => (t.span, UnOp::Neg),
        })? {
            let a = self.prefix()?;

            let span = Span {
                start: op_span.start,
                end: a.span.end,
            };

            Ok(Expr {
                kind: ExprKind::Unary(op, Box::new(a)),
                span,
            })
        } else if let Some((marker_span, marker)) = self.eat(vpred! {
            :t: TokenKind::Val => (t.span, SolveMarker::Val),
            :t: TokenKind::Var => (t.span, SolveMarker::Var),
            :t: TokenKind::Set => (t.span, SolveMarker::Set),
        })? {
            let (name_span, name) = self.require(vpred!(:t: TokenKind::Name(n) => (t.span, n)))?;

            Ok(Expr {
                kind: ExprKind::Solve(marker, name),
                span: Span {
                    start: marker_span.start,
                    end: name_span.end,
                },
            })
        } else {
            self.suffix()
        }
    }

    fn suffix(&mut self) -> Result<'s, Expr<'s>> {
        let Some(mut a) = self.maybe_atom(true)? else {
            return Err(ParseError {
                kind: ParseErrorKind::Unexpected(self.tokens.peek()?.cloned()),
                span: None,
            })
        };

        loop {
            if let Some(arg) = self.maybe_atom(true)? {
                a = Expr {
                    span: Span {
                        start: a.span.start,
                        end: arg.span.end,
                    },
                    kind: ExprKind::Apply(Box::new(a), Box::new(arg)),
                }
            } else {
                break;
            }
        }

        Ok(a)
    }

    fn maybe_atom(&mut self, allow_variants: bool) -> Result<'s, Option<Expr<'s>>> {
        if let Some(open) = self.eat(tpred!(TokenKind::OpenParen))? {
            let scope = self.scope(bpred!(TokenKind::CloseParen))?;
            let close = self.require(tpred!(TokenKind::CloseParen))?;
            Ok(Some(Expr {
                span: Span {
                    start: open.span.start,
                    end: close.span.end,
                },
                kind: scope.kind,
            }))
        } else if allow_variants && self.has_peek(bpred!(TokenKind::Backslash))? {
            Ok(Some(self.variant()?))
        } else if let Some((span, kind)) = self.eat(vpred! {
            :t: TokenKind::Float(f) => (t.span, ExprKind::Literal(Literal::Float(f))),
            :t: TokenKind::Integer(i) => (t.span, ExprKind::Literal(Literal::Integer(i))),
            :t: TokenKind::String(s) => (t.span, ExprKind::Literal(Literal::String(s))),
            :t: TokenKind::Name(n) => (t.span, ExprKind::Name(n)),
        })? {
            Ok(Some(Expr { span, kind }))
        } else {
            Ok(None)
        }
    }

    fn variant(&mut self) -> Result<'s, Expr<'s>> {
        let mut items = Vec::with_capacity(1);
        while let Some(slash) = self.eat(tpred!(TokenKind::Backslash))? {
            let start = slash.span.start;
            let (name_span, name) = self.require(vpred!(:t: TokenKind::Name(n) => (t.span, n)))?;
            let value = self.maybe_atom(false)?;
            let end = value.as_ref().map(|v| v.span.end).unwrap_or(name_span.end);

            items.push(VariantItem {
                name,
                value: value.map(Box::new),
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
                kind: ExprKind::Binary(op, Box::new(a), Box::new(b)),
                span,
            }
        }

        Ok(a)
    }

    fn peek<T>(&mut self, pred: impl Fn(&Token<'s>) -> Option<T>) -> Result<'s, Option<T>> {
        if let Some(token) = self.tokens.peek()? {
            if let Some(t) = pred(token) {
                Ok(Some(t))
            } else {
                Ok(None)
            }
        } else {
            Ok(None)
        }
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
