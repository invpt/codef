use rustc_hash::FxHashMap;

use crate::{reifier::{Symbol, Builtin}, string_storage::Intern};

#[derive(Debug)]
pub struct Module<'s> {
    pub main: Option<Symbol>,
    pub defs: FxHashMap<Symbol, Def<'s>>,
}

#[derive(Debug)]
pub struct Def<'s> {
    pub name: Intern<'s>,
    pub value: Value<'s>
}

#[derive(Debug)]
pub struct Cfg {
    pub args: Box<[TempRef]>,
    pub temps: Box<[TempInfo]>,
    pub blocks: Box<[Block]>,
}

#[derive(Debug)]
pub struct TempInfo {
    pub kind: Kind,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TempRef(pub(super) usize);

#[derive(Debug)]
pub struct Block {
    pub params: Box<[TempInfo]>,
    pub insns: Box<[Insn]>,
    pub branch: Option<Branch<Target>>,
    pub ctrl: Ctrl<Target>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct BlockRef(pub(super) usize);

#[derive(Debug)]
pub struct Target {
    pub block: BlockRef,
    pub arguments: Box<[TempRef]>,
}

#[derive(Debug)]
pub enum Ctrl<Target> {
    Jump(Target),
    Return(TempRef),
}

#[derive(Debug)]
pub struct Branch<Target>(pub BranchCmp, pub TempRef, pub TempRef, pub Target);

#[derive(Debug)]
pub enum BranchCmp {
    Eq,
    Neq,
    Lt,
    Geq,
}

/// Enumeration of all non-control-transfer instructions.
#[derive(Debug)]
pub enum Insn {
    // Copy
    Copy(TempRef, TempRef),

    // Memory instructions
    Load(TempRef, TempRef, u64),
    LoadSym(TempRef, Symbol),
    LoadBuiltin(TempRef, Builtin),
    LoadIr(TempRef, Cfg),
    Store(TempRef, TempRef, u64),

    // Reinterpretation instruactions (int bits <-> float bits)
    FFromIBits(TempRef, TempRef),
    IFromFBits(TempRef, TempRef),

    // Integer instructions (operates on things with kind = Kind::Integer)
    ConstI(TempRef, u64),
    BoolNotI(TempRef, TempRef),
    BitNotI(TempRef, TempRef),
    BitOrI(TempRef, TempRef, TempRef),
    BitXorI(TempRef, TempRef, TempRef),
    BitAndI(TempRef, TempRef, TempRef),
    BitShlI(TempRef, TempRef, TempRef),
    BitShrI(TempRef, TempRef, TempRef),
    NegI(TempRef, TempRef),
    AddI(TempRef, TempRef, TempRef),
    SubI(TempRef, TempRef, TempRef),
    MulI(TempRef, TempRef, TempRef),
    DivI(TempRef, TempRef, TempRef),
    ModI(TempRef, TempRef, TempRef),
    EqI(TempRef, TempRef, TempRef),
    NeqI(TempRef, TempRef, TempRef),
    LtI(TempRef, TempRef, TempRef),
    LeqI(TempRef, TempRef, TempRef),

    // Floating-point instructions (operates on things with kind = Kind::Float)
    ConstF(TempRef, f64),
    AddF(TempRef, TempRef, TempRef),
    SubF(TempRef, TempRef, TempRef),
    MulF(TempRef, TempRef, TempRef),
    DivF(TempRef, TempRef, TempRef),
    EqF(TempRef, TempRef, TempRef),
    NeqF(TempRef, TempRef, TempRef),
    LtF(TempRef, TempRef, TempRef),
    LeqF(TempRef, TempRef, TempRef),

    // Function call
    Call(TempRef, TempRef, Box<[TempRef]>),
}

/// The *kind* of data that is stored in an individual place accessible by the program.
#[derive(Debug)]
pub enum Kind {
    Integer,
    Float,
}

impl Kind {
    pub fn of(ty: &crate::reifier::Type) -> Kind {
        use crate::reifier::{Type, PrimitiveType};
        match ty {
            Type::Constructor(..) => panic!("attempted to compute kind of constructor"),
            Type::Function(..) => Kind::Integer,
            Type::Primitive(PrimitiveType::Boolean | PrimitiveType::Integer | PrimitiveType::String) => Kind::Integer,
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
