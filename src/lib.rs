extern crate itertools;

#[macro_use]
mod utils;
mod lexer;
mod parser;
mod analyzer;
mod interpreter;
mod rascal;

pub use self::rascal::interpret;
