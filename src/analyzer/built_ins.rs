use super::symbol::Symbol;
use super::symbol::VarSymbol;
use super::symbol::CallableSymbol;

use parser::ast::TypeSpec;

pub fn writeln_procedure() -> Symbol {
    return Symbol::Callable(CallableSymbol::Procedure(String::from("writeln"), vec![VarSymbol::STRING(String::from("text"))]));
}

pub fn readln_procedure() -> Symbol {
    return Symbol::Callable(CallableSymbol::Function(String::from("readln"), vec![], TypeSpec::STRING));
}

pub fn int_to_string_function() -> Symbol {
    return Symbol::Callable(CallableSymbol::Function(String::from("IntToString"), vec![VarSymbol::INTEGER(String::from("value"))], TypeSpec::STRING));
}

pub fn real_to_string_function() -> Symbol {
    return Symbol::Callable(CallableSymbol::Function(String::from("RealToString"), vec![VarSymbol::REAL(String::from("value"))], TypeSpec::STRING));
}