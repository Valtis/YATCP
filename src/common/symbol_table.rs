use crate::common::node_info::{FunctionInfo, DeclarationInfo, StructInfo};

#[derive(Clone, PartialEq, Debug)]
pub enum Symbol {
    Function(FunctionInfo),
    Variable(DeclarationInfo, u32),
    Struct(StructInfo),
}

impl Symbol {
    pub fn name(&self) -> &str {
        match self {
            Symbol::Function(function_info) => function_info.name.as_str(),
            Symbol::Variable(declaration_info, _) => declaration_info.name.as_str(),
            Symbol::Struct(struct_info) => struct_info.name.as_str(),
        }
    }
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

    // FIXME Use hashmap to avoid constant re-iterations of the symbol Vec.
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
                Symbol::Struct(ref info) => {
                    if *info.name == name {
                        return Some(s.clone())
                    }
                }
            }
        }
        None
    }
    pub fn symbols(&self) -> impl Iterator<Item=&Symbol> {
        self.symbols.iter()
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

    pub fn get_declaration_info(&self, name: &str) -> Option<DeclarationInfo> {
        match self.find_symbol(name) {
            Some(Symbol::Variable(info, _)) => Some(info.clone()),
            _ => None,
        }
    }
}
