use rustc_hash::FxHashMap;

use crate::{
    reifier::{Builtin, Symbol},
    string_storage::Intern,
};

#[derive(Debug)]
pub struct Module<'s> {
    pub main: Option<Symbol>,
    pub defs: FxHashMap<Symbol, Def<'s>>,
}

#[derive(Debug)]
pub struct Def<'s> {
    pub name: Intern<'s>,
    pub value: Value<'s>,
}

#[derive(Debug)]
pub struct Cfg {
    pub params: Box<[Temp]>,
    pub blocks: Box<[Block]>,
}

#[derive(Debug, Clone, Copy)]
pub struct TempInfo {
    pub kind: Kind,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Temp {
    pub kind: Kind,
    pub idx: usize,
}

#[derive(Debug)]
pub struct Block {
    pub params: Box<[Temp]>,
    pub insns: Box<[Insn]>,
    pub branch: Option<Branch<Target>>,
    pub ctrl: Ctrl<Target>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct BlockRef(pub(super) usize);

#[derive(Debug)]
pub struct Target {
    pub block: BlockRef,
    pub arguments: Box<[Temp]>,
}

#[derive(Debug)]
pub enum Ctrl<Target> {
    Jump(Target),
    Return(Temp),
}

#[derive(Debug)]
pub struct Branch<Target>(pub BranchCmp, pub Temp, pub Temp, pub Target);

#[derive(Debug, Clone, Copy)]
pub enum BranchCmp {
    Eq,
    Neq,
    Lt,
    Geq,
}

#[derive(Debug)]
pub enum Insn {
    Load(Temp, Producer),
    Store(MemRef, Temp),
}

#[derive(Debug)]
pub enum Producer {
    Memory(Kind, MemRef),
    Symbol(Kind, Symbol),
    Builtin(Builtin),
    Ir(Cfg),
    Copy(Temp),
    Binary(BinOp, Temp, Temp),
    Unary(UnOp, Temp),
    Call(Temp, Box<[Temp]>, Kind),
    ConstI(u64),
    ConstF(f64),
}

impl Producer {
    pub fn result_kind(&self) -> Kind {
        use Kind::*;
        use Producer::*;

        match self {
            Builtin(_) | Ir(_) => Integer,
            Memory(k, _) | Symbol(k, _) | Call(_, _, k) => *k,
            Copy(t) => t.kind,
            Binary(op, _, _) => match op {
                BinOp::BitOrI
                | BinOp::BitXorI
                | BinOp::BitAndI
                | BinOp::BitShlI
                | BinOp::BitShrI
                | BinOp::AddI
                | BinOp::SubI
                | BinOp::MulI
                | BinOp::DivI
                | BinOp::ModI
                | BinOp::EqI
                | BinOp::NeqI
                | BinOp::LtI
                | BinOp::LeqI
                | BinOp::EqF
                | BinOp::NeqF
                | BinOp::LtF
                | BinOp::LeqF => Integer,
                BinOp::AddF | BinOp::SubF | BinOp::MulF | BinOp::DivF => Float,
            },
            Unary(op, _) => match op {
                UnOp::BoolNotI | UnOp::BitNotI | UnOp::NegI => Integer,
                UnOp::NegF => Float,
            },
            ConstI(_) => Integer,
            ConstF(_) => Float,
        }
    }
}

#[derive(Debug, Clone)]
pub struct MemRef(pub Temp, pub u64);

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    BitOrI,
    BitXorI,
    BitAndI,
    BitShlI,
    BitShrI,
    AddI,
    SubI,
    MulI,
    DivI,
    ModI,
    EqI,
    NeqI,
    LtI,
    LeqI,
    AddF,
    SubF,
    MulF,
    DivF,
    EqF,
    NeqF,
    LtF,
    LeqF,
}

#[derive(Debug, Clone, Copy)]
pub enum UnOp {
    BoolNotI,
    BitNotI,
    NegI,
    NegF,
}

/// The *kind* of data that is stored in an individual place accessible by the program.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Kind {
    Integer,
    Float,
}

impl Kind {
    pub fn of(ty: &crate::reifier::Type) -> Kind {
        use crate::reifier::{PrimitiveType, Type};
        match ty {
            Type::Constructor(..) => panic!("attempted to compute kind of constructor"),
            Type::Function(..) => Kind::Integer,
            Type::Primitive(
                PrimitiveType::Boolean | PrimitiveType::Integer | PrimitiveType::String,
            ) => Kind::Integer,
            Type::Primitive(PrimitiveType::Float) => Kind::Float,
            Type::Variant(..) => Kind::Integer,
            Type::Tuple(..) => Kind::Integer,
            Type::Symbol(..) => Kind::Integer, // is this valid?
        }
    }
}

#[derive(Debug)]
pub enum Value<'s> {
    Integer(i64),
    Float(f64),
    Tuple(Box<[Value<'s>]>),
    Variant(Intern<'s>, Option<Box<Value<'s>>>),
    Function(Cfg),
}
