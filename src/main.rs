extern crate rascal;

use rascal::lexer;
use rascal::parser;
use rascal::interpreter;

use std::io;
use std::io::Write;

fn main() {
    loop {
        print!("calc>");
        io::stdout().flush().expect("i/o error");
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_)      => interpret(input),
            Err(error) => println!("error: {}", error),
        }
    }
}

fn interpret(input: String) {
    let source = lexer::Source::new(input.as_str());
    let lexer = lexer::Lexer::new(source);
    let parser = parser::Parser::new(lexer);
    let mut interpreter = interpreter::Interpreter::new(parser);

    match interpreter.interpret() {
        Ok(())     => (),
        Err(e)     => println!("{}", e)
    }
}