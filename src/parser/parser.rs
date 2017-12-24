use lexer::{Lexer, Token};
use super::precedence::Precedence;
use super::parselet::{PrefixParselet, InfixParselet};
use super::ast::{
    Program,
    Block,
    Declarations,
    ProcedureDeclaration,
    FunctionDeclaration,
    FormalParameterList,
    FormalParameters,
    VariableDeclaration,
    TypeSpec,
    Compound,
    Statement,
    IfStatement,
    Assignment,
    Variable,
    FunctionCall,
    CallParameters,
    Expr
};

/// <pre>
///     program               :: PROGRAM variable SEMI block DOT
///     block                 :: declarations compound_statement
///     declarations          :: VAR (variable_declaration)+ (procedure_declaration | function_declaration)* | (procedure_declaration | function_declaration)* | empty
///     procedure_declaration :: PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block
///     function_declaration  :: FUNCTION ID LPAREN formal_parameter_list RPAREN COLON type_spec SEMI block
///     formal_parameter_list :: formal_parameters | formal_parameters SEMI formal_parameter_list | empty
///     formal_parameters     :: ID (COMMA ID)* COLON type_spec
///     variable_declaration  :: ID (COMMA ID)* COLON type_spec SEMI
///     type_spec             :: INTEGER | REAL | BOOLEAN
///     compound_statement    :: BEGIN (statement)* END
///     statement             :: compound_statement | if_statement | assignment_statement | function_call
///     if_statement          :: IF expr THEN compound_statement (ELSE (if_statement | compound_statement))?
///     assignment_statement  :: variable ASSIGN expr SEMI
///     variable              :: ID
///     function_call         :: variable LPAREN (call_parameters)? RPAREN SEMI
///     call_parameters       :: expr (COMMA expr)* | empty
///     expr                  :: unaryop_expr | binop_expr | grouped_expr | function_call | literal | variable
///     unaryop_expr          :: unaryop expr
///     unaryop               :: PLUS | MINUS | NOT
///     binop_expr            :: binop expr
///     binop                 :: PLUS | MINUS | MUL | INTEGER_DIV | FLOAT_DIV | AND | OR |
///                              LESS_THAN | LESS_THAN_OR_EQUAL | GREATER_THAN |
///                              GREATER_THAN_OR_EQUAL | EQUAL | NOT_EQUAL
///     grouped_expr          :: LPAREN expr RPAREN
///     literal               :: INTEGER_CONST | REAL_CONST | BOOLEAN_CONST | STRING_LITERAL
///     empty                 ::
/// </pre>
pub struct Parser<'a> {
    pub lexer: Lexer<'a>
}

impl<'a> Parser<'a> {

    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        return Parser { lexer };
    }

    fn get_prefix_parselet(token: &Token) -> Result<PrefixParselet, String> {
        return match token {
            &Token::INTEGER_CONST(_)
            | &Token::REAL_CONST(_)
            | &Token::STRING_LITERAL(_ )
            | &Token::BOOLEAN_CONST(_)   => Ok(PrefixParselet::Literal),
            &Token::PLUS
            | &Token::MINUS              => Ok(PrefixParselet::UnaryOperator(Precedence::UNARY_NUM as u32)),
            &Token::NOT                  => Ok(PrefixParselet::UnaryOperator(Precedence::UNARY_BOOL as u32)),
            &Token::LPAREN               => Ok(PrefixParselet::Grouping),
            &Token::ID(_)                => Ok(PrefixParselet::Variable),
            _                            => Err(String::from(format!("Expression Parse Error at token {:?}", token)))
        };
    }

    fn get_infix_parselet(token: &Token) -> Result<InfixParselet, String> {
        return match token {
            &Token::PLUS
            | &Token::MINUS                 => Ok(InfixParselet::BinaryOperator(Precedence::SUM as u32)),
            &Token::MULTIPLY
            | &Token::INTEGER_DIV
            | &Token::FLOAT_DIV             => Ok(InfixParselet::BinaryOperator(Precedence::PRODUCT as u32)),
            &Token::AND
            | &Token::OR                    => Ok(InfixParselet::BinaryOperator(Precedence::BINARY_BOOL as u32)),
            &Token::LESS_THAN
            | &Token::LESS_THAN_OR_EQUAL
            | &Token::GREATER_THAN
            | &Token::GREATER_THAN_OR_EQUAL
            | &Token::EQUAL
            | &Token::NOT_EQUAL             => Ok(InfixParselet::BinaryOperator(Precedence::COMPARISON as u32)),
            &Token::LPAREN                  => Ok(InfixParselet::FunctionCall(Precedence::CALL as u32)),
            _                               => Err(String::from(format!("Expression Parse Error at token {:?}", token)))
        };
    }

    fn get_next_precedence(&mut self) -> u32 {
        return match self.lexer.peek() {
            Some(token) => match Parser::get_infix_parselet(token) {
                Ok(parselet) => parselet.get_precedence(),
                Err(_)       => 0
            },
            None        => 0
        };
    }
    /// <pre>
    ///     program :: PROGRAM variable SEMI block DOT
    /// </pre>
    pub fn program(&mut self) -> Result<Program, String> {
        match self.lexer.next() {
            Some(Token::PROGRAM) => Ok(()),
            _                    => Err(String::from("Program Parse Error: Expected token PROGRAM"))
        }?;
        let variable = self.variable()?;
        match self.lexer.next() {
            Some(Token::SEMI) => Ok(()),
            _                 => Err(String::from(format!("Program Parse Error at {:?}: Expected token {:?}", variable, Token::SEMI)))
        }?;
        let block = self.block()?;
        match self.lexer.next() {
            Some(Token::DOT) => Ok(()),
            _                => Err(String::from(format!("Program Parse Error: Expected token {:?}", Token::DOT)))
        }?;

        return Ok(Program::Program(variable, block));
    }

    /// <pre>
    ///     block :: declarations compound_statement
    /// </pre>
    pub fn block(&mut self) -> Result<Block, String> {
        return Ok(Block::Block(self.declarations()?, self.compound_statement()?));
    }

    /// <pre>
    ///     declarations :: VAR (variable_declaration)+ (procedure_declaration | function_declaration)* | (procedure_declaration | function_declaration)* | empty
    /// </pre>
    pub fn declarations(&mut self) -> Result<Vec<Declarations>, String> {
        let mut declarations: Vec<Declarations> = vec![];

        if let Some(&Token::VAR) = self.lexer.peek() {
            self.lexer.next(); // eat the var

            let mut variable_declarations: Vec<VariableDeclaration> = vec![];
            while let Some(&Token::ID(_)) = self.lexer.peek() {
                variable_declarations.push(self.variable_declaration()?);
            }

            if variable_declarations.len() > 0 {
                declarations.push(Declarations::VariableDeclarations(variable_declarations));
            } else {
                return Err(String::from(format!("Declarations Parse Error: Expected at least one variable declaration after token {:?}", Token::VAR)));
            }
        }

        let mut procedure_declarations: Vec<ProcedureDeclaration> = vec![];
        let mut function_declarations: Vec<FunctionDeclaration> = vec![];

        loop {
            match self.lexer.peek() {
                Some(&Token::PROCEDURE) => procedure_declarations.push(self.procedure_declaration()?),
                Some(&Token::FUNCTION)  => function_declarations.push(self.function_declaration()?),
                _                       => break
            }
        }

        if procedure_declarations.len() > 0 {
            declarations.push(Declarations::ProcedureDeclarations(procedure_declarations));
        }

        if function_declarations.len() > 0 {
            declarations.push(Declarations::FunctionDeclarations(function_declarations));
        }

        if declarations.is_empty() {
            declarations.push(Declarations::Empty);
        }

        return Ok(declarations);
    }

    /// <pre>
    ///     variable_declaration :: ID (COMMA ID)* COLON type_spec SEMI
    /// </pre>
    fn variable_declaration(&mut self) -> Result<VariableDeclaration, String> {
        let mut ids: Vec<String> = vec![];

        match self.lexer.next() {
            Some(Token::ID(name)) => {
                ids.push(name);
                Ok(())
            },
            _                     => Err(String::from(format!("Variable Declaration Parse Error: Expected id token")))
        }?;

        while let Some(&Token::COMMA) = self.lexer.peek() {
            self.lexer.next(); // eat the comma

            match self.lexer.next() {
                Some(Token::ID(name)) => {
                    ids.push(name);
                    Ok(())
                },
                _                     => Err(String::from(format!("Variable Declaration Parse Error: Expected id token after token {:?}", Token::COMMA)))
            }?;
        }

        return match self.lexer.next() {
            Some(Token::COLON) => match (self.type_spec()?, self.lexer.next()) {
                (type_spec, Some(Token::SEMI)) => Ok(VariableDeclaration::Variables(ids, type_spec)),
                (type_spec, _)                 => Err(String::from(format!("Variable Declaration Parse Error: Expected {:?} token after {:?}", Token::SEMI, type_spec)))
            },
            _                  => Err(String::from(format!("Variable Declaration Parse Error: Expected {:?} token after declared variables", Token::COLON)))
        };
    }

    /// <pre>
    ///     type_spec:: INTEGER | REAL | STRING | BOOLEAN
    /// </pre>
    fn type_spec(&mut self) -> Result<TypeSpec, String> {
        return match self.lexer.next() {
            Some(Token::INTEGER) => Ok(TypeSpec::INTEGER),
            Some(Token::REAL)    => Ok(TypeSpec::REAL),
            Some(Token::STRING)  => Ok(TypeSpec::STRING),
            Some(Token::BOOLEAN) => Ok(TypeSpec::BOOLEAN),
            _                    => Err(String::from(format!("TypeSpec Parse Error: Expected one of {:?}", vec![TypeSpec::INTEGER, TypeSpec::REAL, TypeSpec::STRING, TypeSpec::BOOLEAN])))
        };
    }

    /// <pre>
    ///     procedure_declaration :: PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block
    /// </pre>
    fn procedure_declaration(&mut self) -> Result<ProcedureDeclaration, String> {
        let name = match (self.lexer.next(), self.lexer.next()) {
            (Some(Token::PROCEDURE), Some(Token::ID(name))) => Ok(name),
            _                                               => Err(String::from(format!("Procedure Declaration Parse Error: Expected {:?} <id>", Token::PROCEDURE)))
        }?;
        let parameters = match self.lexer.peek() {
            Some(&Token::LPAREN) =>  {
                self.lexer.next(); // eat the LPAREN

                match (self.formal_parameter_list()?, self.lexer.next()) {
                    (formal_parameter_list, Some(Token::RPAREN)) => Ok(formal_parameter_list),
                    (formal_parameter_list, _)                   => Err(format!("Procedure Declaration Parse Error: Expected {:?} after {:?}", Token::RPAREN, formal_parameter_list))
                }
            }
            _                   => Ok(FormalParameterList::FormalParameters(vec![]))
        }?;
        let block = match self.lexer.next() {
            Some(Token::SEMI) => self.block(),
            _                 => Err(String::from(format!("Procedure Declaration Parse Error: Expected token {:?} after {:?}", Token::SEMI, parameters)))
        }?;

        return Ok(ProcedureDeclaration::Procedure(name, parameters, block));
    }

    /// <pre>
    ///     function_declaration :: FUNCTION ID LPAREN formal_parameter_list RPAREN COLON type_spec SEMI block
    /// </pre>
    fn function_declaration(&mut self) -> Result<FunctionDeclaration, String> {
        let name = match (self.lexer.next(), self.lexer.next()) {
            (Some(Token::FUNCTION), Some(Token::ID(name))) => Ok(name),
            _                                              => Err(String::from(format!("Function Declaration Parse Error: Expected {:?} <id>", Token::FUNCTION)))
        }?;
        let parameters = match self.lexer.next() {
            Some(Token::LPAREN) => match (self.formal_parameter_list()?, self.lexer.next()) {
                (formal_parameter_list, Some(Token::RPAREN)) => Ok(formal_parameter_list),
                (formal_parameter_list, _)                   => Err(format!("Function Declaration Parse Error: Expected {:?} after {:?}", Token::RPAREN, formal_parameter_list))
            },
            _                   => Err(format!("Function Declaration Parse Error: Expected {:?} after '{:?} {:?}'", Token::LPAREN, Token::FUNCTION, name))
        }?;
        let return_type = match self.lexer.next() {
            Some(Token::COLON) => self.type_spec(),
            _                  => Err(String::from(format!("Function Declaration Parse Error: Expected {:?} after {:?}", Token::COLON, parameters)))
        }?;
        let block = match self.lexer.next() {
            Some(Token::SEMI) => self.block(),
            _                 => Err(String::from(format!("Function Declaration Parse Error: Expected {:?} after {:?}", Token::SEMI, return_type)))
        }?;

        return Ok(FunctionDeclaration::Function(name, parameters, block, return_type));
    }

    /// <pre>
    ///     formal_parameter_list :: formal_parameters | formal_parameters SEMI formal_parameter_list | empty
    /// </pre>
    fn formal_parameter_list(&mut self) -> Result<FormalParameterList, String> {
        let mut parameters: Vec<FormalParameters> = vec![];

        if let Some(&Token::ID(_)) = self.lexer.peek() {
            loop {
                parameters.push(self.formal_parameters()?);

                match self.lexer.peek() {
                    Some(&Token::SEMI) => self.lexer.next(),
                    _                  => break
                };
            }
        }

        return Ok(FormalParameterList::FormalParameters(parameters));
    }

    /// <pre>
    ///     formal_parameters :: ID (COMMA ID)* COLON type_spec
    /// </pre>
    fn formal_parameters(&mut self) -> Result<FormalParameters, String> {
        let mut ids: Vec<String> = vec![];

        match self.lexer.next() {
            Some(Token::ID(name)) => {
                ids.push(name);
                Ok(())
            },
            token                 => Err(String::from(format!("Formal Parameters Parse Error: Expected id token, got token {:?}", token)))
        }?;

        while let Some(&Token::COMMA) = self.lexer.peek() {
            self.lexer.next(); // eat the comma

            match self.lexer.next() {
                Some(Token::ID(name)) => {
                    ids.push(name);
                    Ok(())
                },
                _                     => Err(String::from(format!("Formal Parameters Parse Error: Expected id token after {:?}", Token::COMMA)))
            }?;
        }

        let type_spec = match self.lexer.next() {
            Some(Token::COLON) => self.type_spec(),
            _                  => Err(String::from(format!("Formal Parameters Parse Error: Expected token {:?} after parameter identifiers", Token::COLON)))
        }?;

        return Ok(FormalParameters::Parameters(ids, type_spec));
    }

    /// <pre>
    ///     compound_statement :: BEGIN (statement)* END
    /// </pre>
    fn compound_statement(&mut self) -> Result<Compound, String> {
        match self.lexer.next() {
            Some(Token::BEGIN) => Ok(()),
            _                  => Err(String::from(format!("Compound Statement Parse Error: Expected token {:?}", Token::BEGIN)))
        }?;

        let mut statements: Vec<Statement> = vec![];
        while !matches!(self.lexer.peek(), Some(&Token::END)) {
            statements.push(self.statement()?);
        }
        self.lexer.next(); // eat the 'END' token

        return Ok(Compound::Statements(statements));
    }

    /// <pre>
    ///     statement :: compound_statement | if_statement | assignment_statement | function_call
    /// </pre>
    fn statement(&mut self) -> Result<Statement, String> {
        return match self.lexer.peek() {
            Some(&Token::BEGIN) => Ok(Statement::Compound(self.compound_statement()?)),
            Some(&Token::ID(_)) => match self.lexer.peek_ahead(1) {
                Some(&Token::LPAREN) => Ok(Statement::FunctionCall(self.function_call()?)),
                Some(&Token::ASSIGN) => Ok(Statement::Assignment(self.assignment_statement()?)),
                _                    => Err(String::from("Statement Parse Error: Expected assignment statement or function call after id token"))
            },
            Some(&Token::IF)    => Ok(Statement::IfStatement(self.if_statement()?)),
            _                   => Err(String::from("Statement Parse Error: Expected compound statement, function call, assignment statement, or if statement"))
        };
    }

    /// <pre>
    ///     if_statement :: IF expr THEN compound_statement (ELSE (if_statement | compound_statement))?
    /// </pre>
    fn if_statement(&mut self) -> Result<IfStatement, String> {
        let (if_expr, if_compound) = match self.lexer.next() {
            Some(Token::IF) => match (self.expr(None)?, self.lexer.next()) {
                (expr, Some(Token::THEN)) => Ok((expr, self.compound_statement()?)),
                (expr, _)                 => Err(String::from(format!("If Statement Parse Error: Expected token {:?} after {:?}", Token::THEN, expr)))
            },
            _               => Err(String::from(format!("If statement Parse Error: Expected token {:?}", Token::IF)))
        }?;

        return match self.lexer.peek() {
            Some(&Token::ELSE) => {
                self.lexer.next(); // eat the 'else' token
                match self.lexer.peek() {
                    Some(&Token::IF) => Ok(IfStatement::IfElseIf(if_expr, if_compound, Box::new(self.if_statement()?))),
                    _                => Ok(IfStatement::IfElse(if_expr, if_compound, self.compound_statement()?))
                }
            },
            _                  => Ok(IfStatement::If(if_expr, if_compound))
        };
    }

    /// <pre>
    ///     assignment_statement  :: variable ASSIGN expr SEMI
    /// </pre>
    fn assignment_statement(&mut self) -> Result<Assignment, String> {
        return match (self.variable()?, self.lexer.next()) {
            (var, Some(Token::ASSIGN)) => match (self.expr(None)?, self.lexer.next()) {
                (expr, Some(Token::SEMI)) => Ok(Assignment::Assign(var, expr)),
                (expr, _)                 => Err(String::from(format!("Assignment Statement Parse Error: Expected {:?} after {:?}", Token::SEMI, expr)))
            },
            (var, _)                   => Err(String::from(format!("Assignment Statement Parse Error: Expected {:?} after {:?}", Token::ASSIGN, var)))
        };
    }

    /// <pre>
    ///     variable :: ID
    /// </pre>
    fn variable(&mut self) -> Result<Variable, String> {
        return match self.lexer.next() {
            Some(Token::ID(id)) => Ok(Variable::Var(id)),
            _                   => Err(String::from("Variable Parse Error: Expected id token"))
        };
    }

    /// <pre>
    ///     function_call :: variable LPAREN (call_parameters)? RPAREN SEMI
    /// </pre>
    pub fn function_call(&mut self) -> Result<FunctionCall, String> {
        let function_id = match (self.variable()?, self.lexer.next()) {
            (variable, Some(Token::LPAREN)) => Ok(variable),
            (variable, _)                   => Err(String::from(format!("Function Call Parse Error: Expected token {:?} after {:?}", Token::LPAREN, variable)))
        }?;

        return match (self.call_parameters()?, self.lexer.next(), self.lexer.next()) {
            (call_parameters, Some(Token::RPAREN), Some(Token::SEMI)) => Ok(FunctionCall::Call(function_id, call_parameters)),
            (call_parameters, _, _)                                   => Err(String::from(format!("Function Call Parse Error: Expected tokens {:?} {:?} after {:?}", Token::RPAREN, Token::SEMI, call_parameters)))
        };
    }

    /// <pre>
    ///     call_parameters :: expr (COMMA expr)* | empty
    /// </pre>
    pub fn call_parameters(&mut self) -> Result<CallParameters, String> {
        if let Some(&Token::RPAREN) = self.lexer.peek() {
            return Ok(CallParameters::Parameters(vec![]));
        } else {
            let mut parameters = vec![self.expr(None)?];

            while let Some(&Token::COMMA) = self.lexer.peek() {
                self.lexer.next(); // eat the comma
                parameters.push(self.expr(None)?);
            }

            return Ok(CallParameters::Parameters(parameters));
        }
    }

    pub fn expr(&mut self, precedence: Option<u32>) -> Result<Expr, String> {
        let precedence = precedence.unwrap_or(0);
        let token = self.lexer.next().ok_or(String::from("Expression Parse Error: Expected a token, found none"))?;
        let parselet = Parser::get_prefix_parselet(&token)?;

        let mut left = parselet.parse(self, &token)?;

        while precedence < self.get_next_precedence() {
            let token = self.lexer.next().ok_or(String::from("Expression Parse Error: Expected a token, found none"))?;
            let parselet = Parser::get_infix_parselet(&token)?;

            left = parselet.parse(self, &left, &token)?;
        }

        return Ok(left);
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let program = self.program()?;

        match self.lexer.next() {
            Some(Token::EOF) => Ok(()),
            Some(token)      => Err(String::from(format!("Parse Error: Expected {:?} token, found token {:?}", Token::EOF, token))),
            None             => Err(String::from(format!("Parse Error: Expected {:?} token, found None", Token::EOF)))
        }?;

        return Ok(program);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use lexer::Source;
    use parser::ast::UnaryOpExpr;
    use parser::ast::UnaryOperator;
    use parser::ast::BinaryOpExpr;
    use parser::ast::BinaryOperator;
    use parser::ast::GroupedExpr;

    fn get_parser(text: &str) -> Parser {
        let source = Source::new(text);
        let lexer = Lexer::new(source);
        let parser = Parser::new(lexer);

        return parser;
    }

    #[test]
    fn program() {
        let mut parser = get_parser("program test; begin end.");

        assert_matches!(parser.program(), Ok(Program::Program(_, _)));
    }

    #[test]
    fn block() {
        let mut parser = get_parser("begin end");

        assert_matches!(parser.block(), Ok(Block::Block(_, _)));
    }

    #[test]
    fn declarations() {
        let mut parser = get_parser("var test_var: integer; function test_func(): string; begin end procedure test_proc; begin end");
        match parser.declarations() {
            Ok(declarations) => {
                assert_eq!(declarations.len(), 3);
                assert_matches!(declarations[0], Declarations::VariableDeclarations(_));
                assert_matches!(declarations[1], Declarations::ProcedureDeclarations(_));
                assert_matches!(declarations[2], Declarations::FunctionDeclarations(_));
            },
            Err(e)           => panic!(e)
        };
    }

    #[test]
    fn variable_declaration() {
        let mut parser = get_parser("foo, bar, baz : integer;");
        match parser.variable_declaration() {
            Ok(VariableDeclaration::Variables(names, _)) => assert_eq!(names, vec![String::from("foo"), String::from("bar"), String::from("baz")]),
            Err(e)                                       => panic!(e)
        }
    }

    #[test]
    fn type_spec() {
        let mut parser = get_parser("integer real string boolean");

        assert_matches!(parser.type_spec(), Ok(TypeSpec::INTEGER));
        assert_matches!(parser.type_spec(), Ok(TypeSpec::REAL));
        assert_matches!(parser.type_spec(), Ok(TypeSpec::STRING));
        assert_matches!(parser.type_spec(), Ok(TypeSpec::BOOLEAN));
    }

    #[test]
    fn procedure_declaration() {
        let mut parser = get_parser("procedure test(); begin end");
        match parser.procedure_declaration() {
            Ok(ProcedureDeclaration::Procedure(name, _, _)) => assert_eq!(name, String::from("test")),
            Err(e)                                          => panic!(e)
        }
    }

    #[test]
    fn function_declaration() {
        let mut parser = get_parser("function test(): integer; begin end");
        match parser.function_declaration() {
            Ok(FunctionDeclaration::Function(name, _, _, _)) => assert_eq!(name, String::from("test")),
            Err(e)                                           => panic!(e)
        }
    }

    #[test]
    fn formal_parameters_list() {
        let mut parser = get_parser("a: integer; b: real; c, d: string");

        assert_matches!(parser.formal_parameter_list(), Ok(FormalParameterList::FormalParameters(_)));
    }

    #[test]
    fn formal_parameters() {
        let mut parser = get_parser("a, b, c: integer");
        match parser.formal_parameters() {
            Ok(FormalParameters::Parameters(names, _)) => assert_eq!(names, vec![String::from("a"), String::from("b"), String::from("c")]),
            Err(e)                                     => panic!(e)
        };
    }

    #[test]
    fn compound_statement() {
        let mut parser = get_parser("begin end");

        assert_matches!(parser.compound_statement(), Ok(Compound::Statements(_)));
    }

    #[test]
    fn if_statement() {
        let mut parser = get_parser("if true then begin end");

        assert_matches!(parser.if_statement(), Ok(IfStatement::If(_, _)));
    }

    #[test]
    fn if_else_statement() {
        let mut parser = get_parser("if false then begin end else begin end");

        assert_matches!(parser.if_statement(), Ok(IfStatement::IfElse(_, _, _)));
    }

    #[test]
    fn if_else_if_statement() {
        let mut parser = get_parser("if false then begin end else if true then begin end else if true then begin end");
        match parser.if_statement() {
            Ok(IfStatement::IfElseIf(_, _, else_if_one)) => match *else_if_one {
                IfStatement::IfElseIf(_, _, else_if_two) => match *else_if_two {
                    IfStatement::If(_, _) => (),
                    _                     => panic!("Failure: Expected IfStatement::If AST node")
                },
                _                                        => panic!("Failure: Expected IfStatement::IfElseIf AST node")
            },
            _                                            => panic!("Failure: Expected IfStatement::IfElseIf AST node")
        };
    }

    #[test]
    fn if_else_if_else_statement() {
        let mut parser = get_parser("if false then begin end else if false then begin end else begin end");
        match parser.if_statement() {
            Ok(IfStatement::IfElseIf(_, _, else_if)) => match *else_if {
                IfStatement::IfElse(_, _, _) => (),
                _                            => panic!("Failure: Expected IfStatement::IfElse AST node")
            },
            _                                        => panic!("Failure: Expected IfStatement::IfElseIf AST node")
        };
    }

    #[test]
    fn assignment_statement() {
        let mut parser = get_parser("foo := 5;");

        assert_matches!(parser.assignment_statement(), Ok(Assignment::Assign(_, _)));
    }

    #[test]
    fn variable() {
        let mut parser = get_parser("foo");
        match parser.variable() {
            Ok(Variable::Var(name)) => assert_eq!(name, String::from("foo")),
            Err(e)                  => panic!(e)
        };
    }

    #[test]
    fn function_call() {
        let mut parser = get_parser("test(foo, bar, 5 + 5);");

        assert_matches!(parser.function_call(), Ok(FunctionCall::Call(_, _)));
    }

    #[test]
    fn call_parameters() {
        let mut parser = get_parser("foo, bar, 5 + 5");

        assert_matches!(parser.call_parameters(), Ok(CallParameters::Parameters(_)));
    }

    #[test]
    fn expr_unary_plus() {
        let mut parser = get_parser("+5");
        match parser.expr(Some(0)) {
            Ok(Expr::UnaryOp(unary_expr)) => match *unary_expr {
                UnaryOpExpr::UnaryOp(UnaryOperator::Plus, _) => (),
                _                                            => panic!("Failure: Expected unary plus AST node")
            },
            _                             => panic!("Failure: Expected unary expression AST node")
        };
    }

    #[test]
    fn expr_unary_minus() {
        let mut parser = get_parser("-5");
        match parser.expr(Some(0)) {
            Ok(Expr::UnaryOp(unary_expr)) => match *unary_expr {
                UnaryOpExpr::UnaryOp(UnaryOperator::Minus, _) => (),
                _                                             => panic!("Failure: Expected unary minus AST node")
            },
            _                             => panic!("Failure: Expected unary expression AST node")
        };
    }

    #[test]
    fn expr_binary_add() {
        let mut parser = get_parser("5 + 5");
        match parser.expr(Some(0)) {
            Ok(Expr::BinOp(binop_expr)) => match *binop_expr {
                BinaryOpExpr::BinaryOp(_, BinaryOperator::Plus, _) => (),
                _                                                  => panic!("Failure: Expected binary addition AST node")
            },
            _                           => panic!("Failure: Expected binary expression AST node")
        };
    }

    #[test]
    fn expr_binary_subtract() {
        let mut parser = get_parser("5 - 5");
        match parser.expr(Some(0)) {
            Ok(Expr::BinOp(binop_expr)) => match *binop_expr {
                BinaryOpExpr::BinaryOp(_, BinaryOperator::Minus, _) => (),
                _                                                   => panic!("Failure: Expected binary subtraction AST node")
            },
            _                           => panic!("Failure: Expected binary expression AST node")
        };
    }

    #[test]
    fn expr_binary_multiply() {
        let mut parser = get_parser("5 * 5");
        match parser.expr(Some(0)) {
            Ok(Expr::BinOp(binop_expr)) => match *binop_expr {
                BinaryOpExpr::BinaryOp(_, BinaryOperator::Multiply, _) => (),
                _                                                      => panic!("Failure: Expected binary multiply AST node")
            },
            _                           => panic!("Failure: Expected binary expression AST node")
        };
    }

    #[test]
    fn expr_binary_int_divide() {
        let mut parser = get_parser("5 div 5");
        match parser.expr(Some(0)) {
            Ok(Expr::BinOp(binop_expr)) => match *binop_expr {
                BinaryOpExpr::BinaryOp(_, BinaryOperator::IntegerDivide, _) => (),
                _                                                           => panic!("Failure: Expected binary integer division AST node")
            },
            _                           => panic!("Failure: Expected binary expression AST node")
        };
    }

    #[test]
    fn expr_binary_float_divide() {
        let mut parser = get_parser("5 / 5");
        match parser.expr(Some(0)) {
            Ok(Expr::BinOp(binop_expr)) => match *binop_expr {
                BinaryOpExpr::BinaryOp(_, BinaryOperator::FloatDivide, _) => (),
                _                                                         => panic!("Failure: Expected binary float division AST node")
            },
            _                           => panic!("Failure: Expected binary expression AST node")
        };
    }

    #[test]
    fn expr_grouped() {
        let mut parser = get_parser("(5 + 5)");
        match parser.expr(Some(0)) {
            Ok(Expr::Group(grouped_expr)) => match *grouped_expr {
                GroupedExpr::Group(_) => ()
            },
            _                             => panic!("Failure: Expected grouped expression AST node")
        };
    }

    #[test]
    fn expr_function_call() {
        let mut parser = get_parser("test()");

        assert_matches!(parser.expr(Some(0)), Ok(Expr::FunctionCall(_)));
    }

    #[test]
    fn expr_literal() {
        for literal in vec!["5", "5.5", "'test'", "true"] {
            let mut parser = get_parser(literal);

            assert_matches!(parser.expr(Some(0)), Ok(Expr::Literal(_)));
        }
    }

    #[test]
    fn expr_variable() {
        let mut parser = get_parser("test");

        assert_matches!(parser.expr(Some(0)), Ok(Expr::Variable(_)));
    }

    #[test]
    fn parse() {
        let mut parser = get_parser("program test; begin end.");

        assert_matches!(parser.parse(), Ok(Program::Program(_, _)));
    }
}