use super::symbol::Symbol;
use super::symbol::ProcedureSymbol;
use super::symbol::VarSymbol;

pub fn writeln_procedure() -> Symbol {
    return Symbol::Procedure(ProcedureSymbol::Procedure(String::from("writeln"), vec![VarSymbol::STRING(String::from("text"))]))
}