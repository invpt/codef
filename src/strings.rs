use std::{
    cell::UnsafeCell,
    hash::Hash,
};

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

pub struct Strings {
    strings: UnsafeCell<FxHashSet<&'static str>>,
}

impl Strings {
    pub fn new() -> Strings {
        Strings {
            strings: UnsafeCell::new(FxHashSet::default()),
        }
    }

    pub fn intern<'a>(&'a self, s: Box<str>) -> Intern<'a> {
        let to_intern = Box::leak(s) as &'static str;
        let strings = unsafe { &mut *self.strings.get() };
        if let Some(interned) = strings.get(to_intern) {
            // drop the string we were going to intern
            // SAFETY: we just got this reference from Box::leak
            unsafe {
                drop(Box::from_raw(to_intern as *const str as *mut str));
            }

            Intern(interned)
        } else {
            strings.insert(to_intern);
            Intern(to_intern)
        }
    }
}

impl Drop for Strings {
    fn drop(&mut self) {
        let strings = std::mem::take(self.strings.get_mut());
        for interned in strings {
            // SAFETY: all references to the string are gone now since we
            //         have a mutable reference to Strings
            unsafe {
                drop(Box::from_raw(interned as *const str as *mut str));
            }
        }
    }
}
