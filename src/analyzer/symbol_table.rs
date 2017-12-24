use std::fmt;
use std::fmt::Debug;
use std::collections::HashMap;
use super::symbol::Symbol;

pub struct SymbolTable {
    name: String,
    symbols: HashMap<String, Symbol>,
    enclosing_scope: Option<Box<SymbolTable>>
}

impl SymbolTable {

    pub fn new(name: String) -> SymbolTable {
        return SymbolTable { name, symbols: HashMap::new(), enclosing_scope: None }
    }

    pub fn with_enclosing_scope(name: String, enclosing_scope: SymbolTable) -> SymbolTable {
        return SymbolTable { name, symbols: HashMap::new(), enclosing_scope: Some(Box::new(enclosing_scope))}
    }

    pub fn name(&self) -> String {
        return self.name.clone();
    }

    pub fn enclosing_scope(self) -> Option<SymbolTable> {
        return match self.enclosing_scope {
            Some(scope) => Some(*scope),
            None        => None
        };
    }

    pub fn define(&mut self, symbol: Symbol) {
        self.symbols.insert(symbol.name(), symbol);
    }

    pub fn lookup(&mut self, name: &String) -> Option<&Symbol> {
        if let Some(symbol) = self.symbols.get(name) {
            return Some(symbol);
        }
        else if let Some(ref mut scope) = self.enclosing_scope {
            return scope.lookup(name);
        }
        else {
            return None;
        }
    }

    pub fn local_lookup(&mut self, name: &String) -> Option<&Symbol> {
        return self.symbols.get(name);
    }
}

impl Debug for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let enclosing_scope_name = match self.enclosing_scope {
            Some(ref table) => Some(table.name()),
            None            => None
        };

        write!(f, "\n\tName: {:?}\n\tSymbols: {:?}\n\tEnclosing Scope: {:?}", self.name, self.symbols, enclosing_scope_name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use analyzer::symbol::VarSymbol;

    #[test]
    fn lookup() {
        let mut symbol_table = SymbolTable::new(String::from("test_table"));
        symbol_table.define(Symbol::Var(VarSymbol::INTEGER(String::from("test"))));

        assert_eq!(
            symbol_table.lookup(&String::from("test")),
            Some(&Symbol::Var(VarSymbol::INTEGER(String::from("test"))))
        );
    }

    #[test]
    fn enclosing_lookup() {
        let mut enclosing_table = SymbolTable::new(String::from("test_enclosing_table"));
        enclosing_table.define(Symbol::Var(VarSymbol::INTEGER(String::from("test"))));
        let mut symbol_table = SymbolTable::with_enclosing_scope(String::from("test_table"), enclosing_table);

        assert_eq!(
            symbol_table.lookup(&String::from("test")),
            Some(&Symbol::Var(VarSymbol::INTEGER(String::from("test"))))
        );
    }

    #[test]
    fn local_lookup() {
        let mut enclosing_table = SymbolTable::new(String::from("test_enclosing_table"));
        enclosing_table.define(Symbol::Var(VarSymbol::INTEGER(String::from("test"))));
        let mut symbol_table = SymbolTable::with_enclosing_scope(String::from("test_table"), enclosing_table);

        assert_eq!(symbol_table.local_lookup(&String::from("test")), None);
    }
}