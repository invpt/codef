use crate::tokenizer::{Intern, Span};

#[derive(Debug)]
pub struct Expr<'s> {
    pub kind: ExprKind<'s>,
    pub span: Span,
}

#[derive(Debug)]
pub enum ExprKind<'s> {
    Object(Box<Scope<'s>>),
    Block(Box<Scope<'s>>),
    Lambda {
        arg: Box<Expr<'s>>,
        body: Box<Expr<'s>>,
    },
    BinOp {
        op: BinOp,
        lhs: Box<Expr<'s>>,
        rhs: Box<Expr<'s>>,
    },
    UnOp {
        op: UnOp,
        arg: Box<Expr<'s>>,
    },
    Access {
        expr: Box<Expr<'s>>,
        prop: AccessRhs<'s>,
    },
    Branch {
        cond: Box<Expr<'s>>,
        on_true: Box<Expr<'s>>,
        on_false: Option<Box<Expr<'s>>>,
    },
    Tuple {
        items: Box<[Expr<'s>]>,
    },
    Apply {
        a: Box<Expr<'s>>,
        b: Box<Expr<'s>>,
    },
    TypeAssertion {
        a: Box<Expr<'s>>,
        b: Box<Expr<'s>>,
    },
    Variant(Box<[VariantItem<'s>]>),
    Ident(Intern<'s>),
    Literal(Literal<'s>),
}

#[derive(Debug)]
pub struct Scope<'s> {
    pub defs: Box<[Def<'s>]>,
    pub body: Box<[Expr<'s>]>,
    pub trailing_semi: bool,
}

#[derive(Debug)]
pub struct VariantItem<'s> {
    pub name: Intern<'s>,
    pub value: Option<Expr<'s>>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Def<'s> {
    pub name: Intern<'s>,
    pub value: Box<Expr<'s>>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Literal<'s> {
    Float(f64),
    Integer(u64),
    String(Intern<'s>),
}

#[derive(Debug)]
pub enum BinOp {
    Equal,
    NotEqual,
    Gt,
    GtEq,
    Lt,
    LtEq,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
}

#[derive(Debug)]
pub enum UnOp {
    Not,
    Set,
    Val,
    Ref,
    Deref,
}

#[derive(Debug)]
pub enum AccessRhs<'s> {
    Prop(Intern<'s>),
    Expr(Box<Expr<'s>>),
}
