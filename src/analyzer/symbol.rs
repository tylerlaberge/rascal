use std::clone::Clone;

use parser::ast::TypeSpec;

#[derive(Debug, Clone, PartialEq)]
pub enum Symbol {
    Var(VarSymbol),
    Callable(CallableSymbol)
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarSymbol {
    INTEGER(String),
    REAL(String),
    STRING(String),
    BOOLEAN(String)
}

#[derive(Debug, Clone, PartialEq)]
pub enum CallableSymbol {
    Procedure(String, Vec<VarSymbol>),
    Function(String, Vec<VarSymbol>, TypeSpec)
}

impl Symbol {

    pub fn name(&self) -> String {
        return match self {
            &Symbol::Var(VarSymbol::INTEGER(ref name))
            | &Symbol::Var(VarSymbol::REAL(ref name))
            | &Symbol::Var(VarSymbol::STRING(ref name))
            | &Symbol::Var(VarSymbol::BOOLEAN(ref name))
            | &Symbol::Callable(CallableSymbol::Procedure(ref name, _))
            | &Symbol::Callable(CallableSymbol::Function(ref name, _, _)) => name.clone()
        };
    }
}