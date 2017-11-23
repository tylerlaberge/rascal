use std::clone::Clone;

use parser::ast::TypeSpec;

#[derive(Debug)]
pub enum Symbol {
    Var(VarSymbol),
    Callable(CallableSymbol)
}

#[derive(Debug)]
pub enum VarSymbol {
    INTEGER(String),
    REAL(String),
    STRING(String),
    BOOLEAN(String)
}

#[derive(Debug)]
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

impl Clone for Symbol {
    fn clone(&self) -> Self {
        return match self {
            &Symbol::Var(ref var_symbol)             => Symbol::Var(var_symbol.clone()),
            &Symbol::Callable(ref callable_symbol)   => Symbol::Callable(callable_symbol.clone())
        }
    }
}
impl Clone for VarSymbol {
    fn clone(&self) -> Self {
        return match self {
            &VarSymbol::INTEGER(ref name) => VarSymbol::INTEGER(name.clone()),
            &VarSymbol::REAL(ref name)    => VarSymbol::REAL(name.clone()),
            &VarSymbol::STRING(ref name)  => VarSymbol::STRING(name.clone()),
            &VarSymbol::BOOLEAN(ref name) => VarSymbol::BOOLEAN(name.clone())
        }
    }
}

impl Clone for CallableSymbol {
    fn clone(&self) -> Self {
        return match self {
            &CallableSymbol::Procedure(ref name, ref params)                 => CallableSymbol::Procedure(name.clone(), params.to_vec()),
            &CallableSymbol::Function(ref name, ref params, ref return_type) => CallableSymbol::Function(name.clone(), params.to_vec(), return_type.clone())
        }
    }
}