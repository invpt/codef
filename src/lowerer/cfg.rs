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
    Call(TempId, TempId, Box<[TempId]>),
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
    // Copy
    Copy(TempId, TempId),

    // Memory instructions
    Load(TempId, TempId, u64),
    Store(TempId, TempId, u64),

    // Reinterpretation instruactions (int bits <-> float bits)
    FFromIBits(TempId, TempId),
    IFromFBits(TempId, TempId),

    // Integer instructions (operates on things with kind = Kind::Integer)
    ConstI(TempId, u64),
    BoolNotI(TempId, TempId),
    BitNotI(TempId, TempId),
    BitOrI(TempId, TempId, TempId),
    BitXorI(TempId, TempId, TempId),
    BitAndI(TempId, TempId, TempId),
    BitShlI(TempId, TempId, TempId),
    BitShrI(TempId, TempId, TempId),
    NegI(TempId, TempId),
    AddI(TempId, TempId, TempId),
    SubI(TempId, TempId, TempId),
    MulI(TempId, TempId, TempId),
    DivI(TempId, TempId, TempId),
    ModI(TempId, TempId, TempId),
    EqI(TempId, TempId, TempId),
    NeqI(TempId, TempId, TempId),
    LtI(TempId, TempId, TempId),
    LeqI(TempId, TempId, TempId),

    // Floating-point instructions (operates on things with kind = Kind::Float)
    ConstF(TempId, f64),
    AddF(TempId, TempId, TempId),
    SubF(TempId, TempId, TempId),
    MulF(TempId, TempId, TempId),
    DivF(TempId, TempId, TempId),
    EqF(TempId, TempId, TempId),
    NeqF(TempId, TempId, TempId),
    LtF(TempId, TempId, TempId),
    LeqF(TempId, TempId, TempId),
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
