use rustc_hash::FxHashMap;

use crate::tokenizer::{Intern, Span};

#[derive(Debug)]
pub struct Module<'s> {
    pub defs: FxHashMap<DefRef, Proc<'s>>,
}

#[derive(Debug)]
pub struct Proc<'s> {
    pub name: Intern<'s>,
    pub vars: FxHashMap<VarRef, Var<'s>>,
    pub stmts: Box<[Stmt<'s>]>,
}

#[derive(Debug)]
pub struct Stmt<'s> {
    kind: StmtKind<'s>,
    span: Option<Span>,
}

#[derive(Debug)]
pub enum StmtKind<'s> {
    Case {
        cond: Expr<'s>,
        on_true: Box<[Stmt<'s>]>,
        on_false: Box<[Stmt<'s>]>,
    },
    For {
        init: Option<Box<Stmt<'s>>>,
        cond: Expr<'s>,
        afterthought: Option<Box<Stmt<'s>>>,
        body: Box<[Stmt<'s>]>,
    },
    Init {
        var: VarRef,
        value: Expr<'s>,
    },
    Set {
        var: VarRef,
        value: Expr<'s>,
    },
    Return(ExprKind<'s>),
    Expr(ExprKind<'s>),
}

#[derive(Debug)]
pub struct Var<'s> {
    name: Intern<'s>,
    mutable: bool,
    ty: Option<Expr<'s>>
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct VarRef(pub(super) usize);

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct DefRef(pub(super) usize);


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
        arg: Option<Box<Expr<'s>>>,
        spec: bool,
        ty: Option<Box<Expr<'s>>>,
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
    Solve(Solve, Box<Expr<'s>>),
    Literal(Literal<'s>),
    Name(Intern<'s>),
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
pub enum Solve {
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
