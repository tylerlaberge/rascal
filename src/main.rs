extern crate rascal;

use rascal::lexer;
use rascal::parser;
use rascal::interpreter;

fn main() {
    let input = "
PROGRAM test;
VAR
   a, b, y : INTEGER;
   y       : REAL;
BEGIN
   a := 2;
   b := 10 * a + 10 * a div 3;
   y := 20 / 7 + 3.14;
END.
";
    println!("{}", input);

    interpret(String::from(input));
}

fn interpret(input: String) {
    let source = lexer::Source::new(input.as_str());
    let lexer = lexer::Lexer::new(source);
    let parser = parser::Parser::new(lexer);
    let mut interpreter = interpreter::Interpreter::new(parser);

    match interpreter.interpret() {
        Ok(())     => println!("{}", interpreter),
        Err(e)     => println!("{}", e)
    }
}