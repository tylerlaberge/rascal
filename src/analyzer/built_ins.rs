use super::symbol::Symbol;
use super::symbol::VarSymbol;
use super::symbol::CallableSymbol;

pub fn writeln_procedure() -> Symbol {
    return Symbol::Callable(CallableSymbol::Procedure(String::from("writeln"), vec![VarSymbol::STRING(String::from("text"))]))
}