use std::{marker::PhantomData, alloc::Layout, rc::Rc};

use rustc_hash::FxHashMap;

use crate::{reifier::Symbol, string_storage::Intern};

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
    pub temps: Box<[Kind]>,
    pub blocks: Box<[Block]>,
}

impl std::ops::Index<BlockRef> for Cfg {
    type Output = Block;

    fn index(&self, index: BlockRef) -> &Block {
        &self.blocks[index.0]
    }
}

impl std::ops::IndexMut<BlockRef> for Cfg {
    fn index_mut(&mut self, index: BlockRef) -> &mut Block {
        &mut self.blocks[index.0]   
    }
}

#[derive(Debug)]
pub struct Temp {
    pub id: TempId,
    pub kind: Kind,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TempId(pub(super) usize);

#[derive(Debug)]
pub struct Block {
    pub params: Box<[Temp]>,
    pub insns: Box<[Insn]>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct BlockRef(pub(super) usize);

#[derive(Debug)]
pub struct Target {
    pub block: BlockRef,
    pub arguments: Box<[TempId]>,
}

/// Linear three address code.
#[derive(Debug)]
pub struct Tac {
    pub params: Box<[Kind]>,
    pub ret: Option<Kind>,
    pub insns: Box<[AnyInsn<usize>]>,
}

#[derive(Debug)]
pub enum AnyInsn<Target> {
    Insn(Insn),
    Ctrl(CtrlInsn<Target>),
}

#[derive(Debug)]
pub enum CtrlInsn<Target> {
    Jump(Target),
    Return(Option<TempId>),
    Branch(BranchCmp, TempId, TempId, Target),
}

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
    // Memory instructions
    Load(DtSa),
    Store(DaSt),

    // Reinterpretation instruactions (int bits <-> float bits)
    FFromIBits(DtSt),
    IFromFBits(DtSt),

    // Integer instructions (operates on things with kind = Kind::Integer)
    ConstI(DtSc<u64>),
    BitNotI(DtSt),
    BitOrI(DtStSt),
    BitXorI(DtStSt),
    BitAndI(DtStSt),
    BitShlI(DtStSt),
    BitShrI(DtStSt),
    AddI(DtStSt),
    SubI(DtStSt),
    MulI(DtStSt),
    DivI(DtStSt),
    ModI(DtStSt),

    // Floating-point instructions (operates on things with kind = Kind::Float)
    ConstF(DtSc<f64>),
    AddF(DtStSt),
    SubF(DtStSt),
    MulF(DtStSt),
    DivF(DtStSt),
}

/// Two-arg instructions with a destination temp and source address.
#[derive(Debug, PartialEq, Eq)]
pub struct DtSa {
    pub dest: TempId,
    pub src_addr: TempId,
}

/// Two-arg instructions with a destination address and source address.
#[derive(Debug, PartialEq, Eq)]
pub struct DaSt {
    pub dest_addr: TempId,
    pub src: TempId,
}

/// Two-arg instructions with a destination and one source.
#[derive(Debug, PartialEq, Eq)]
pub struct DtSt {
    pub dest: TempId,
    pub src: TempId,
}

/// Two-arg instructions with a destination and a source constant.
#[derive(Debug, PartialEq, Eq)]
pub struct DtSc<C> {
    pub dest: TempId,
    pub cnst: C,
}


/// Three-arg instructions with a destination and two sources.
#[derive(Debug, PartialEq, Eq)]
pub struct DtStSt {
    pub dest: TempId,
    pub src1: TempId,
    pub src2: TempId,
}

/// The *kind* of data that is stored in an individual place accessible by the program.
#[derive(Debug)]
pub enum Kind {
    Integer,
    Float,
}

#[derive(Debug)]
pub enum Value<'s> {
    Integer(i64),
    Float(f64),
    Tuple(Box<[Value<'s>]>),
    Variant(Intern<'s>, Option<Box<Value<'s>>>),
    Function(Box<[Kind]>, Option<Kind>, Cfg),
}
