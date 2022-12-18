use rustc_hash::FxHashSet;

use crate::string_storage::StringStorage;

use super::Intern;

/// Interns strings so that each unique string interned by a given instance of
/// this struct has a single unique address in memory.
pub struct StringInterner<'s> {
    storage: &'s StringStorage,
    strings: FxHashSet<&'s str>,
}

impl<'s> StringInterner<'s> {
    /// Creates a new string interner.
    pub fn new(storage: &'s StringStorage) -> StringInterner<'s> {
        StringInterner {
            storage,
            strings: FxHashSet::default()
        }
    }

    /// Takes ownership of the given string and interns it.
    pub fn intern(&mut self, s: String) -> Intern<'s> {
        if let Some(s) = self.strings.get(&*s) {
            Intern(s)
        } else {
            let stored = self.storage.store(s.into());
            self.strings.insert(stored);
            Intern(stored)
        }
    }
}
