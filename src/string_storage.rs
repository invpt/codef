use std::{cell::RefCell, hash::Hash};

use rustc_hash::FxHashSet;

#[derive(Debug, Clone, Copy)]
pub struct Intern<'s>(pub &'s str);

impl<'s> PartialEq for Intern<'s> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}

impl<'s> Eq for Intern<'s> {}

impl<'s> Hash for Intern<'s> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_usize(self.0 as *const str as *const u8 as usize)
    }
}

// Interns strings so that each unique string interned by a given instance of
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
            strings: FxHashSet::default(),
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


/// Storage for strings interned by a [StringInterner]. StringInterners just
/// need a reference to one of these so that they can keep track of all the
/// strings that are stored. The real purpose of this struct is to allow the
/// StringInterner to pass out references to strings while also allowing itself
/// to be borrowed mutably at the same time to intern new strings.
pub struct StringStorage {
    strings: RefCell<Vec<*mut str>>,
}

impl StringStorage {
    pub fn new() -> StringStorage {
        StringStorage {
            strings: RefCell::new(Vec::new()),
        }
    }

    pub fn store(&self, string: Box<str>) -> &str {
        let string = Box::leak(string);

        let mut strings = self.strings.borrow_mut();
        strings.push(string as *mut _);

        string
    }
}

impl Drop for StringStorage {
    fn drop(&mut self) {
        let strings = self.strings.borrow();
        for string in &*strings {
            // SAFETY: each of these pointers was returned by Box::leak in `store`,
            //         and the lifetime given out for these strings is only valid
            //         for as long as the StringStorage stays alive, so these
            //         strings should no longer be referenced.
            let string: *mut str = *string;
            unsafe {
                drop(Box::from_raw(string));
            }
        }
    }
}
