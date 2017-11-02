extern crate rascal;

use rascal::lexer;
use rascal::parser;
use rascal::analyzer;
use rascal::interpreter;

fn main() {
    let input = "
PROGRAM test;
VAR
   a, b : INTEGER;
   y    : REAL;
BEGIN
   a := 2;
   b := 10 * a + 10 * a div 3;
   y := 20.0 / 7.0 + 3.14;
END.
";
    println!("{}", input);

    interpret(String::from(input));
}

fn interpret(input: String) {
    let source = lexer::Source::new(input.as_str());
    let lexer = lexer::Lexer::new(source);
    let mut parser = parser::Parser::new(lexer);

    match parser.parse() {
        Ok(program) => {
            let mut analyzer = analyzer::SemanticAnalyzer::new();
            let mut interpreter = interpreter::Interpreter::new();

            analyzer.analyze(&program);
            interpreter.interpret(&program);
        },
        Err(e)     => println!("{}", e)
    };
}