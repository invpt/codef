use std::num::NonZeroUsize;

use rustc_hash::FxHashMap;

use crate::string_storage::Intern;

use super::Symbol;

pub struct Scoper<'s> {
    stack: Vec<FxHashMap<Intern<'s>, Symbol>>,
    sym_counter: NonZeroUsize,
}

impl<'s> Scoper<'s> {
    pub fn lookup(&mut self, name: Intern<'s>) -> Option<Symbol> {
        for scope in self.stack.iter().rev() {
            if let Some(symb) = scope.get(&name) {
                return Some(*symb)
            }
        }

        None
    }

    pub fn new_anonymous_symbol(&mut self) -> Symbol {
        let index = self.sym_counter;
        if let Some(incremented) = self.sym_counter.checked_add(1) {
            self.sym_counter = incremented;
        } else {
            panic!("Symbol counter overflowed!")
        }
        let sym = Symbol(index);
        sym
    }

    pub fn new_symbol(&mut self, name: Intern<'s>) -> Symbol {
        let sym = self.new_anonymous_symbol();
        self.stack.last_mut().unwrap().insert(name, sym);
        sym
    }

    pub fn push(&mut self) {
        self.stack.push(FxHashMap::default())
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }
}

impl<'s> Default for Scoper<'s> {
    fn default() -> Self {
        Scoper { stack: vec![], sym_counter: NonZeroUsize::new(1).unwrap() }
    }
}
