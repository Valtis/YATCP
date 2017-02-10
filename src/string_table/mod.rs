use std::collections::HashMap;
use std::rc::Rc;
// string interning
// provides O(1) insertion and lookup on strings
// currently string is stored twice (once in hashmap, once in vector).
// this could be optimized
pub struct StringTable {
    strings: Vec<Rc<String>>,
    str_indices: HashMap<String, usize>,
}


impl StringTable {
    pub fn new() -> StringTable {
        StringTable {
            strings: vec![],
            str_indices: HashMap::new(),
        }
    }

    pub fn insert(&mut self, string: String) -> Rc<String> {
        if self.str_indices.contains_key(&string) {
            return self.strings[self.str_indices[&string]].clone();
        }

        let index = self.strings.len();
        self.str_indices.insert(string.clone(), index);
        self.strings.push(Rc::new(string));
        self.strings[index].clone()
    }
}

#[test]
fn inserting_multiple_into_empty_table_returns_successive_indexes() {
    let mut table = StringTable::new();
    assert_eq!(Rc::new("hello".to_string()), table.insert("hello".to_string()));
    assert_eq!(Rc::new("aa".to_string()), table.insert("aa".to_string()));
    assert_eq!(Rc::new("bb".to_string()), table.insert("bb".to_string()));
    assert_eq!(Rc::new("cc".to_string()), table.insert("cc".to_string()));
    assert_eq!(Rc::new("dd".to_string()), table.insert("dd".to_string()));
}


#[test]
fn inserting_same_string_multiple_times_returns_same_pointer() {
    let mut table = StringTable::new();
    let orig_hello = table.insert("hello".to_string());
    assert!(rc_ptr_eq(&orig_hello, &table.insert("hello".to_string())));
}


// Rc::ptr_eq has not yet been stabilized when writing this. Replace this helper
// with Rc::ptr_eq once this is no longer the case
pub fn rc_ptr_eq(this: &Rc<String>, other: &Rc<String>) -> bool {
    let this_ptr: *const String = &**this;
    let other_ptr: *const String = &**other;
    this_ptr == other_ptr
}