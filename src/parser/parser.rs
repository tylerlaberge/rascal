use lexer::Lexer;
use lexer::Token;
use super::precedence::Precedence;
use super::parselet::PrefixParselet;
use super::parselet::InfixParselet;
use super::ast::Program;
use super::ast::Block;
use super::ast::Declarations;
use super::ast::ProcedureDeclaration;
use super::ast::FunctionDeclaration;
use super::ast::FormalParameterList;
use super::ast::FormalParameters;
use super::ast::VariableDeclaration;
use super::ast::TypeSpec;
use super::ast::Compound;
use super::ast::StatementList;
use super::ast::Statement;
use super::ast::Assignment;
use super::ast::Variable;
use super::ast::FunctionCall;
use super::ast::CallParameters;
use super::ast::Expr;

/// <pre>
///     program               :: PROGRAM variable SEMI block DOT
///     block                 :: declarations compound_statement
///     declarations          :: VAR (variable_declaration)+ (procedure_declaration | function_declaration)* | (procedure_declaration | function_declaration)* | empty
///     procedure_declaration :: PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI
///     function_declaration  :: FUNCTION ID LPAREN formal_parameter_list RPAREN COLON type_spec SEMI block SEMI
///     formal_parameter_list :: formal_parameters | formal_parameters SEMI formal_parameter_list | empty
///     formal_parameters     :: ID (COMMA ID)* COLON type_spec
///     variable_declaration  :: ID (COMMA ID)* COLON type_spec SEMI
///     type_spec             :: INTEGER | REAL
///     compound_statement    :: BEGIN statement_list END
///     statement_list        :: statement | statement SEMI statement_list
///     statement             :: compound_statement | assignment_statement | function_call | empty
///     assignment_statement  :: variable ASSIGN expr
///     variable              :: ID
///     function_call         :: variable LPAREN (call_parameters)? RPAREN
///     call_parameters       :: expr | expr COMMA call_parameters
///     expr                  :: unaryop_expr | binop_expr | grouped_expr | function_call | literal | variable
///     unaryop_expr          :: (PLUS | MINUS) expr
///     binop_expr            :: expr (PLUS | MINUS | MUL | INTEGER_DIV | FLOAT_DIV) expr
///     grouped_expr          :: LPAREN expr RPAREN
///     literal               :: INTEGER_CONST | REAL_CONST | STRING_LITERAL
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
            | &Token::STRING_LITERAL(_ ) => Ok(PrefixParselet::Literal),
            &Token::PLUS
            | &Token::MINUS              => Ok(PrefixParselet::UnaryOperator(Precedence::UNARY as u32)),
            &Token::LPAREN               => Ok(PrefixParselet::Grouping),
            &Token::ID(_)                => Ok(PrefixParselet::Variable),
            _                            => Err(String::from(format!("{:?} is not a prefix token", token)))
        };
    }

    fn get_infix_parselet(token: &Token) -> Result<InfixParselet, String> {
        return match token {
            &Token::PLUS
            | &Token::MINUS       => Ok(InfixParselet::BinaryOperator(Precedence::SUM as u32)),
            &Token::MULTIPLY
            | &Token::INTEGER_DIV
            | &Token::FLOAT_DIV   => Ok(InfixParselet::BinaryOperator(Precedence::PRODUCT as u32)),
            &Token::LPAREN        => Ok(InfixParselet::FunctionCall(Precedence::CALL as u32)),
            _                     => Err(String::from(format!("{:?} is not an infix token", token)))
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
    fn program(&mut self) -> Result<Program, String> {
        match (self.lexer.next(), self.variable()?, self.lexer.next(), self.block()?, self.lexer.next()) {
            (Some(Token::PROGRAM), variable, Some(Token::SEMI), block, Some(Token::DOT)) => Ok(Program::Program(variable, block)),
            _                                                                            => Err(String::from("Program Parse Error"))
        }
    }

    /// <pre>
    ///     block :: declarations compound_statement
    /// </pre>
    fn block(&mut self) -> Result<Block, String> {
        return Ok(Block::Block(self.declarations()?, self.compound_statement()?));
    }

    /// <pre>
    ///     declarations :: VAR (variable_declaration)+ (procedure_declaration | function_declaration)* | (procedure_declaration | function_declaration)* | empty
    /// </pre>
    fn declarations(&mut self) -> Result<Vec<Declarations>, String> {
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
                return Err(String::from("Declarations Parse Error"));
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
            _                     => Err(String::from("Variable Declaration Parse Error"))
        }?;

        while let Some(&Token::COMMA) = self.lexer.peek() {
            self.lexer.next(); // eat the comma

            match self.lexer.next() {
                Some(Token::ID(name)) => {
                    ids.push(name);
                    Ok(())
                },
                _                     => Err(String::from("Variable Declaration Parse Error"))
            }?;
        }

        return match (self.lexer.next(), self.type_spec()?, self.lexer.next()) {
            (Some(Token::COLON), type_spec, Some(Token::SEMI)) => Ok(VariableDeclaration::Variables(ids, type_spec)),
            _                                                  => Err(String::from("Variable Declaration Parse Error"))
        };
    }

    /// <pre>
    ///     type_spec:: INTEGER | REAL
    /// </pre>
    fn type_spec(&mut self) -> Result<TypeSpec, String> {
        return match self.lexer.next() {
            Some(Token::INTEGER) => Ok(TypeSpec::INTEGER),
            Some(Token::REAL)    => Ok(TypeSpec::REAL),
            Some(Token::STRING)  => Ok(TypeSpec::STRING),
            _                    => Err(String::from("TypeSpec Parse Error"))
        };
    }

    /// <pre>
    ///     procedure_declaration :: PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI
    /// </pre>
    fn procedure_declaration(&mut self) -> Result<ProcedureDeclaration, String> {
        let name = match (self.lexer.next(), self.lexer.next()) {
            (Some(Token::PROCEDURE), Some(Token::ID(name))) => Ok(name),
            _                                               => Err(String::from("Procedure Declaration Parse Error"))
        }?;
        let parameters = match self.lexer.peek() {
            Some(&Token::LPAREN) =>
                match (self.lexer.next(), self.formal_parameter_list()?, self.lexer.next()) {
                    (Some(Token::LPAREN), formal_parameter_list, Some(Token::RPAREN)) => Ok(formal_parameter_list),
                    _                                                                 => Err(String::from("Procedure Declaration Parse Error"))
            },
            _                   => Ok(FormalParameterList::FormalParameters(vec![]))
        }?;
        let block = match (self.lexer.next(), self.block()?, self.lexer.next()) {
            (Some(Token::SEMI), block, Some(Token::SEMI)) => Ok(block),
            _                                             => Err(String::from("Procedure Declaration Parse Error"))
        }?;

        return Ok(ProcedureDeclaration::Procedure(name, parameters, block));
    }

    /// <pre>
    ///     function_declaration :: FUNCTION ID LPAREN formal_parameter_list RPAREN COLON type_spec SEMI block SEMI
    /// </pre>
    fn function_declaration(&mut self) -> Result<FunctionDeclaration, String> {
        let name = match (self.lexer.next(), self.lexer.next()) {
            (Some(Token::FUNCTION), Some(Token::ID(name))) => Ok(name),
            _                                              => Err(String::from("Function Declaration Parse Error"))
        }?;
        let parameters = match (self.lexer.next(), self.formal_parameter_list()?, self.lexer.next()) {
            (Some(Token::LPAREN), formal_parameter_list, Some(Token::RPAREN)) => Ok(formal_parameter_list),
            _                                                                 => Err(String::from("Function Declaration Parse Error"))
        }?;
        let return_type = match (self.lexer.next(), self.type_spec()?) {
            (Some(Token::COLON), type_spec) => Ok(type_spec),
            _                               => Err(String::from("Function Declaration Parse Error"))
        }?;
        let block = match (self.lexer.next(), self.block()?, self.lexer.next()) {
            (Some(Token::SEMI), block, Some(Token::SEMI)) => Ok(block),
            _                                             => Err(String::from("Function Declaration Parse Error"))
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
            _                     => Err(String::from("Formal Parameters Parse Error"))
        }?;

        while let Some(&Token::COMMA) = self.lexer.peek() {
            self.lexer.next(); // eat the comma

            match self.lexer.next() {
                Some(Token::ID(name)) => {
                    ids.push(name);
                    Ok(())
                },
                _                     => Err(String::from("Formal Parameters Parse Error"))
            }?;
        }

        return match (self.lexer.next(), self.type_spec()?) {
            (Some(Token::COLON), type_spec) => Ok(FormalParameters::Parameters(ids, type_spec)),
            _                               => Err(String::from("Formal Parameters Parse Error"))
        };
    }

    /// <pre>
    ///     compound_statement :: BEGIN statement_list END
    /// </pre>
    fn compound_statement(&mut self) -> Result<Compound, String> {
        match self.lexer.next() {
            Some(Token::BEGIN) => Ok(()),
            _                  => Err("Compound Statement Parse Error")
        }?;

        let statement_list = self.statement_list()?;

        return match self.lexer.next() {
            Some(Token::END)   => Ok(Compound::StatementList(statement_list)),
            _                  => Err(String::from("Compound Statement Parse Error"))
        };
    }

    /// <pre>
    ///     statement_list :: statement | statement SEMI statement_list
    /// </pre>
    fn statement_list(&mut self) -> Result<StatementList, String> {
        let mut statements: Vec<Statement> = vec![];
        loop {
            statements.push(self.statement()?);

            match self.lexer.peek() {
                Some(&Token::SEMI) => {
                    self.lexer.next(); // eat the semicolon

                    if let Some(&Token::END) = self.lexer.peek() {
                        break;
                    }
                },
                _                  => break
            };
        }

        return Ok(StatementList::Statements(statements));
    }

    /// <pre>
    ///     statement :: compound_statement | assignment_statement | function_call | empty
    /// </pre>
    fn statement(&mut self) -> Result<Statement, String> {
        return match self.lexer.peek() {
            Some(&Token::BEGIN) => Ok(Statement::Compound(self.compound_statement()?)),
            Some(&Token::ID(_)) => match self.lexer.peek_ahead(1) {
                Some(&Token::LPAREN) => Ok(Statement::FunctionCall(self.function_call()?)),
                _                    => Ok(Statement::Assignment(self.assignment_statement()?))
            },
            Some(&Token::END)   => {
                self.lexer.next(); // eat the token

                Ok(Statement::Empty)
            },
            _                   => Err(String::from("Statement Parse Error"))
        };
    }

    /// <pre>
    ///     assignment_statement :: variable ASSIGN expr
    /// </pre>
    fn assignment_statement(&mut self) -> Result<Assignment, String> {
        return match (self.variable()?, self.lexer.next(), self.expr(None)?) {
            (var, Some(Token::ASSIGN), expr) => Ok(Assignment::Assign(var, expr)),
            _                                => Err(String::from("Assignment Parse Error"))
        }
    }

    /// <pre>
    ///     variable :: ID
    /// </pre>
    fn variable(&mut self) -> Result<Variable, String> {
        return match self.lexer.next() {
            Some(Token::ID(id)) => Ok(Variable::Var(id)),
            _                   => Err(String::from("Variable Parse Error"))
        };
    }

    /// <pre>
    ///     function_call :: variable LPAREN (call_parameters)? RPAREN
    /// </pre>
    fn function_call(&mut self) -> Result<FunctionCall, String> {
        let function_id = self.variable()?;

        match self.lexer.next() {
            Some(Token::LPAREN) => Ok(()),
            _                   => Err(String::from("Function Call Parse Error"))
        }?;

        let function_params = if let Some(&Token::RPAREN) = self.lexer.peek() {
            CallParameters::Parameters(vec![])
        } else {
            self.call_parameters()?
        };

        match self.lexer.next() {
            Some(Token::RPAREN) => Ok(()),
            _                   => Err(String::from("Function Call Parse Error"))
        }?;

        return Ok(FunctionCall::Call(function_id, function_params));
    }

    /// <pre>
    ///     call_parameters :: expr | expr COMMA call_parameters
    /// </pre>
    pub fn call_parameters(&mut self) -> Result<CallParameters, String> {
        let mut parameters = vec![self.expr(None)?];

        while let Some(&Token::COMMA) = self.lexer.peek() {
            self.lexer.next(); // eat the comma
            parameters.push(self.expr(None)?);
        }

        return Ok(CallParameters::Parameters(parameters));
    }

    pub fn expr(&mut self, precedence: Option<u32>) -> Result<Expr, String> {
        let precedence = precedence.unwrap_or(0);
        let token = match self.lexer.next() {
            Some(t) => Ok(t),
            None    => Err(String::from("Expression Parse Error"))
        }?;
        let parselet = Parser::get_prefix_parselet(&token)?;

        let mut left = parselet.parse(self, &token)?;

        while precedence < self.get_next_precedence() {
            let token = match self.lexer.next() {
                Some(t) => Ok(t),
                None    => Err(String::from("Expression Parse Error"))
            }?;
            let parselet = Parser::get_infix_parselet(&token)?;
            left = parselet.parse(self, &left, &token)?;
        }

        return Ok(left);
    }

    pub fn parse(&mut self) -> Result<Program, String> {
        let program = self.program()?;

        match self.lexer.next() {
            Some(Token::EOF) => Ok(()),
            Some(token)      => Err(format!("Parse Error: Expected EOF token, found '{:?}'", token)),
            None             => Err(String::from("Internal Parse Error"))
        }?;

        return Ok(program);
    }
}

