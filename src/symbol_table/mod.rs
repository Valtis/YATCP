use std::collections::HashMap;
use ast::FunctionInfo;
use semcheck::Type;

#[derive(Clone, PartialEq)]
pub enum SymbolType {
    Function(FunctionInfo),
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
    // I would really like to use LinkedHashMap here, but as of writing this, it does not
    // work on a stable compiler
    symbols: Vec<Symbol>, 
}

impl TableEntry {
    pub fn new() -> TableEntry {
        TableEntry {
            symbols: vec![],
        }
    }

    pub fn add_symbol(&mut self, symbol: Symbol) {
        self.symbols.push(symbol);
    }

    pub fn find_symbol(&self, name: &String) -> Option<Symbol> {
        for ref s in self.symbols.iter() {
            if s.name == *name {
                return Some((*s).clone());
            }
        }
        None
    }

    pub fn find_enclosing_function_info(&self) -> Option<FunctionInfo> {
       for ref s in self.symbols.iter().rev() {
            if let SymbolType::Function(ref info) = s.symbol_type {
                return Some(info.clone())
            }
        }
        None
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

    pub fn get_enclosing_function_info(&self) -> FunctionInfo {
        for i in self.entries.iter().rev() {
            if let Some(entry) = i.find_enclosing_function_info() {
                return entry;
            }
        }
        panic!("Internal compiler error: No enclosing function was found");
    }
}
