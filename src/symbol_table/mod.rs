use crate::ast::{FunctionInfo, DeclarationInfo};

#[derive(Clone, PartialEq, Debug)]
pub enum Symbol {
    Function(FunctionInfo),
    Variable(DeclarationInfo, u32),
}

#[derive(Debug, Clone)]
pub struct TableEntry {
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

    pub fn find_symbol(&self, name: &str) -> Option<Symbol> {
        for s in self.symbols.iter() {
            match *s {
                Symbol::Function(ref info) => {
                    if *info.name == name {
                        return Some(s.clone());
                    }
                },
                Symbol::Variable(ref info, _) => {
                    if *info.name == name {
                        return Some(s.clone());
                    }
                },
            }
        }
        None
    }

    pub fn find_enclosing_function_info(&self) -> Option<FunctionInfo> {
       for s in self.symbols.iter().rev() {
            if let &Symbol::Function(ref info) = s {
                return Some(info.clone())
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    entries: Vec<TableEntry>
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            entries: vec![],
        }
    }

    pub fn push_empty(&mut self) {
        self.entries.push(TableEntry::new());
    }

    pub fn push(&mut self, entry: TableEntry) {
        self.entries.push(entry);
    }

    pub fn pop(&mut self) -> Option<TableEntry> {
        self.entries.pop()
    }

    pub fn top(&mut self) -> Option<TableEntry> {
        if self.entries.is_empty() {
            None
        } else {
            Some(self.entries.last().unwrap().clone())
        }

    }

    pub fn add_symbol(&mut self, symbol: Symbol) {
        assert!(self.entries.len() > 0);
        let last = self.entries.len() - 1;
        self.entries[last].add_symbol(symbol);
    }

    pub fn find_symbol(&self, name: &str) -> Option<Symbol> {
        for i in self.entries.iter().rev() {
            if let Some(entry) = i.find_symbol(name) {
                return Some(entry);
            }
        }
        None
    }

    pub fn get_enclosing_function_info(&self) -> Option<FunctionInfo> {
        for i in self.entries.iter().rev() {
            if let Some(entry) = i.find_enclosing_function_info() {
                return Some(entry);
            }
        }
        None
    }
}
