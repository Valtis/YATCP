use std::collections::HashMap;
use semcheck::Type;
#[derive(Clone, PartialEq)]
pub enum SymbolType {
    Function,
    Variable(Type),
}

#[derive(Clone)]
pub struct Symbol {
    pub line: i32,
    pub column: i32,
    pub name: String,
    pub symbol_type: SymbolType
}

impl Symbol {
    pub fn new(name: String, line: i32, column: i32, symbol_type:SymbolType) -> Symbol {
        Symbol {
            line: line,
            column: column,
            name: name,
            symbol_type: symbol_type,
        }
    }
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

    pub fn add_symbol(&mut self, symbol: Symbol) {
        self.symbols.insert(symbol.name.clone(), symbol);
    }

    pub fn find_symbol(&self, name: &String) -> Option<Symbol> {
        match self.symbols.get(name) {
            Some(x) => Some((*x).clone()),
            None =>None,
        }

    }

    pub fn find_symbol_ref(&self, name: &String) -> Option<&Symbol> {
        self.symbols.get(name)
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

    pub fn add_symbol(&mut self, symbol: Symbol) {
        assert!(self.entries.len() > 0);
        let last = self.entries.len() - 1;
        self.entries[last].add_symbol(symbol);
    }

    pub fn find_symbol(&self, name: &String) -> Option<Symbol> {
        for i in self.entries.iter().rev() {
            if let Some(entry) = i.find_symbol(name) {
                return Some(entry);
            }
        }
        None
    }


    pub fn find_symbol_ref(&self, name: &String) -> Option<&Symbol> {
        for i in self.entries.iter().rev() {
            if let Some(entry) = i.find_symbol_ref(name) {
                return Some(entry);
            }
        }
        None
    }
}
