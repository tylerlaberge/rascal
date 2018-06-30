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
            Symbol::Var(VarSymbol::INTEGER(name))
            | Symbol::Var(VarSymbol::REAL(name))
            | Symbol::Var(VarSymbol::STRING(name))
            | Symbol::Var(VarSymbol::BOOLEAN(name))
            | Symbol::Callable(CallableSymbol::Procedure(name, _))
            | Symbol::Callable(CallableSymbol::Function(name, _, _)) => name.clone()
        };
    }
}