use parser::ast::TypeSpec;
use super::symbol::{Symbol, VarSymbol, CallableSymbol};

pub fn write_procedure() -> Symbol {
    return Symbol::Callable(CallableSymbol::Procedure(String::from("write"), vec![VarSymbol::STRING(String::from("text"))]));
}

pub fn writeln_procedure() -> Symbol {
    return Symbol::Callable(CallableSymbol::Procedure(String::from("writeln"), vec![VarSymbol::STRING(String::from("text"))]));
}

pub fn readln_function() -> Symbol {
    return Symbol::Callable(CallableSymbol::Function(String::from("readln"), vec![], TypeSpec::STRING));
}

pub fn int_to_string_function() -> Symbol {
    return Symbol::Callable(CallableSymbol::Function(String::from("IntToString"), vec![VarSymbol::INTEGER(String::from("value"))], TypeSpec::STRING));
}

pub fn real_to_string_function() -> Symbol {
    return Symbol::Callable(CallableSymbol::Function(String::from("RealToString"), vec![VarSymbol::REAL(String::from("value"))], TypeSpec::STRING));
}

pub fn string_to_int_function() -> Symbol {
    return Symbol::Callable(CallableSymbol::Function(String::from("StringToInt"), vec![VarSymbol::STRING(String::from("text"))], TypeSpec::INTEGER));
}

pub fn string_to_real_function() -> Symbol {
    return Symbol::Callable(CallableSymbol::Function(String::from("StringToReal"), vec![VarSymbol::STRING(String::from("text"))], TypeSpec::REAL));
}