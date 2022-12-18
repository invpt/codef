use std::cell::RefCell;

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
        StringStorage { strings: RefCell::new(Vec::new()) }
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