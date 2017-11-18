use std::clone::Clone;


#[derive(Debug)]
pub enum Symbol {
    Var(VarSymbol),
    Procedure(ProcedureSymbol)
}

#[derive(Debug)]
pub enum VarSymbol {
    INTEGER(String),
    REAL(String),
    STRING(String)
}

#[derive(Debug)]
pub enum ProcedureSymbol {
    Procedure(String, Vec<VarSymbol>)
}

impl Symbol {

    pub fn name(&self) -> String {
        return match self {
            &Symbol::Var(VarSymbol::INTEGER(ref name))
            | &Symbol::Var(VarSymbol::REAL(ref name))
            | &Symbol::Var(VarSymbol::STRING(ref name))
            | &Symbol::Procedure(ProcedureSymbol::Procedure(ref name, _)) => name.clone()
        };
    }
}

impl Clone for Symbol {
    fn clone(&self) -> Self {
        return match self {
            &Symbol::Var(ref var_symbol)             => Symbol::Var(var_symbol.clone()),
            &Symbol::Procedure(ref procedure_symbol) => Symbol::Procedure(procedure_symbol.clone())
        }
    }
}
impl Clone for VarSymbol {
    fn clone(&self) -> Self {
        return match self {
            &VarSymbol::INTEGER(ref name) => VarSymbol::INTEGER(name.clone()),
            &VarSymbol::REAL(ref name)    => VarSymbol::REAL(name.clone()),
            &VarSymbol::STRING(ref name)  => VarSymbol::STRING(name.clone())
        }
    }
}

impl Clone for ProcedureSymbol {
    fn clone(&self) -> Self {
        return match self {
            &ProcedureSymbol::Procedure(ref name, ref params) => ProcedureSymbol::Procedure(name.clone(), params.to_vec())
        }
    }
}