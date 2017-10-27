extern crate rascal;

use rascal::lexer;
use rascal::parser;
use rascal::interpreter;

fn main() {
    let input = "
BEGIN
    BEGIN
        number := 2;
        a := number;
        b := 10 * a + 10 * number / 4;
        c := a - - b
    END;
    x := 11;
END.";
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