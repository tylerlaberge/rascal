use std::collections::HashMap;
use super::symbol::Symbol;

#[derive(Debug)]
pub struct SymbolTable {
    symbols: HashMap<String, Symbol>
}

impl SymbolTable {

    pub fn new() -> SymbolTable {
        return SymbolTable { symbols: HashMap::new() }
    }

    pub fn define(&mut self, symbol: Symbol) {
        self.symbols.insert(symbol.name(), symbol);
    }

    pub fn lookup(&mut self, name: &String) -> Option<&Symbol> {
        return self.symbols.get(name);
    }
}