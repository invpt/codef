use crate::tokenizer::{Intern, Span};

#[derive(Debug)]
pub struct Scope<'s> {
    pub defs: Box<[Def<'s>]>,
    pub exprs: Box<[Expr<'s>]>,
    pub discard: bool,
}

#[derive(Debug)]
pub struct Def<'s> {
    pub name: Intern<'s>,
    pub value: Box<Expr<'s>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Expr<'s> {
    pub kind: ExprKind<'s>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind<'s> {
    Scope(Scope<'s>),
    Abstract {
        spec: bool,
        arg: Option<Box<Expr<'s>>>,
        body: Box<Expr<'s>>,
        ty: Option<Box<Expr<'s>>>,
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
    Assert {
        expr: Box<Expr<'s>>,
        ty: Box<Expr<'s>>,
    },
    Binary(BinOp, Box<Expr<'s>>, Box<Expr<'s>>),
    Unary(UnOp, Box<Expr<'s>>),
    Apply(Box<Expr<'s>>, Box<Expr<'s>>),
    Solve(SolveMarker, Intern<'s>),
    Name(Intern<'s>),
    Literal(Literal<'s>),
}

#[derive(Debug)]
pub enum BinOp {
    Or,
    And,
    Eq,
    Neq,
    Lt,
    Leq,
    Gt,
    Geq,
    Recv,
    BitOr,
    BitXor,
    BitAnd,
    Shl,
    Shr,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum UnOp {
    Not,
    Neg,
}

#[derive(Debug)]
pub enum SolveMarker {
    Val,
    Var,
    Set,
}

#[derive(Debug)]
pub enum Literal<'s> {
    Float(f64),
    Integer(u64),
    Variant(Intern<'s>),
    String(Intern<'s>),
}
