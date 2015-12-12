use std::collections::HashMap;

struct Symbol {
    initialized: bool,

}

struct TableEntry {
    symbols: HashMap<String, Symbol>,
}

impl TableEntry {
    pub fn new() -> TableEntry {
        TableEntry {
            symbols: HashMap::new(),
        }
    }
}

pub struct SymbolTable {
    entries: Vec<TableEntry> 
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            entries: vec![],
        }
    }

    pub fn push(&mut self) {
        self.entries.push(TableEntry::new());
    }

    pub fn pop(&mut self) {
        self.entries.pop();
    }

   /* pub fn add_symbol(symbol: Symbol) {
        assert!(entries.len() > 0);
        entries[entries.len() - 1 ].add_symbol(symbol);
    }*/
}


