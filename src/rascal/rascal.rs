use lexer::{Source, Lexer};
use parser::Parser;
use analyzer::SemanticAnalyzer;
use interpreter::Interpreter;

pub fn interpret(source_text: String) {
    let source = Source::new(source_text.as_str());
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    match parser.parse() {
        Ok(program) => {
            let mut analyzer = SemanticAnalyzer::new();
            let mut interpreter = Interpreter::new();

            analyzer.analyze(&program);
            interpreter.interpret(&program);
        },
        Err(e)     => println!("{}", e)
    };
}