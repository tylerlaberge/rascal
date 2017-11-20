use lexer::Lexer;
use lexer::Token;
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
use super::ast::Literal;
use super::ast::BinaryOperator;
use super::ast::UnaryOperator;

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
///     expr                  :: add_expr
///     add_expr              :: mult_expr (( PLUS | MINUS ) mult_expr )*
///     mult_expr             :: unary_expr (( MUL | INTEGER_DIV | FLOAT_DIV ) unary_expr)*
///     unary_expr            :: (PLUS | MINUS) unary_expr | primary_expr
///     primary_expr          :: literal | grouping | variable | function_call
///     literal               :: INTEGER_CONST | REAL_CONST | STRING_LITERAL
///     grouping              :: LPAREN expr RPAREN
/// </pre>
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {

    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        return Parser { lexer };
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
        return match (self.variable()?, self.lexer.next(), self.expr()?) {
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
    fn call_parameters(&mut self) -> Result<CallParameters, String> {
        let mut parameters = vec![self.expr()?];

        while let Some(&Token::COMMA) = self.lexer.peek() {
            self.lexer.next(); // eat the comma
            parameters.push(self.expr()?);
        }

        return Ok(CallParameters::Parameters(parameters));
    }

    /// <pre>
    ///     expr :: add_expr
    /// </pre>
    fn expr(&mut self) -> Result<Expr, String> {
        return self.add_expr();
    }

    /// <pre>
    ///     mult_expr (( PLUS | MINUS ) mult_expr )*
    /// </pre>
    fn add_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.mult_expr()?;

        while matches!(self.lexer.peek(), Some(&Token::PLUS) | Some(&Token::MINUS)) {
            expr = match (self.lexer.next(), self.mult_expr()?) {
                (Some(Token::PLUS), other_expr)  => Ok(Expr::BinOp(Box::new(expr), BinaryOperator::Plus, Box::new(other_expr))),
                (Some(Token::MINUS), other_expr) => Ok(Expr::BinOp(Box::new(expr), BinaryOperator::Minus, Box::new(other_expr))),
                _                                => Err(String::from("Additive Expression Parse Error"))
            }?;
        }

        return Ok(expr);
    }

    /// <pre>
    ///     mult_expr :: unary_expr (( MUL | INTEGER_DIV | FLOAT_DIV ) unary_expr)*
    /// </pre>
    fn mult_expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.unary_expr()?;

        while matches!(self.lexer.peek(), Some(&Token::MULTIPLY) | Some(&Token::INTEGER_DIV) | Some(&Token::FLOAT_DIV)) {
            expr = match (self.lexer.next(), self.unary_expr()?) {
                (Some(Token::MULTIPLY), other_expr)    => Ok(Expr::BinOp(Box::new(expr), BinaryOperator::Multiply, Box::new(other_expr))),
                (Some(Token::INTEGER_DIV), other_expr) => Ok(Expr::BinOp(Box::new(expr), BinaryOperator::IntegerDivide, Box::new(other_expr))),
                (Some(Token::FLOAT_DIV), other_expr)   => Ok(Expr::BinOp(Box::new(expr), BinaryOperator::FloatDivide, Box::new(other_expr))),
                _                                      => Err(String::from("Multiplicative Expression Parse Error"))
            }?;
        }

        return Ok(expr);
    }

    /// <pre>
    ///     unary_expr :: (PLUS | MINUS) unary_expr | primary_expr
    /// </pre>
    fn unary_expr(&mut self) -> Result<Expr, String> {
        return match self.lexer.peek() {
            Some(&Token::PLUS)  => {
                self.lexer.next();
                Ok(Expr::UnaryOp(UnaryOperator::Plus, Box::new(self.unary_expr()?)))
            },
            Some(&Token::MINUS) => {
                self.lexer.next();
                Ok(Expr::UnaryOp(UnaryOperator::Minus, Box::new(self.unary_expr()?)))
            },
            _                   => self.primary_expr()
        };
    }

    /// <pre>
    ///     primary_expr :: literal | grouping | variable | function_call
    /// </pre>
    fn primary_expr(&mut self) -> Result<Expr, String> {
        return match self.lexer.peek() {
            Some(&Token::ID(_))               => match self.lexer.peek_ahead(1) {
                Some(&Token::LPAREN)             => Ok(Expr::FunctionCall(self.function_call()?)),
                _                                => Ok(Expr::Variable(self.variable()?))
            },
            Some(&Token::LPAREN)              => self.grouping(),
            Some(&Token::INTEGER_CONST(_))
            | Some(&Token::REAL_CONST(_))
            | Some(&Token::STRING_LITERAL(_)) => Ok(Expr::Literal(self.literal()?)),
            _                                 => Err(String::from("Primary Expression Parse Error"))
        };
    }

    /// <pre>
    ///     literal :: INTEGER_CONST | REAL_CONST | STRING_LITERAL
    /// </pre>
    fn literal(&mut self) -> Result<Literal, String> {
        return match self.lexer.next() {
            Some(Token::INTEGER_CONST(i))  => Ok(Literal::Int(i)),
            Some(Token::REAL_CONST(i))     => Ok(Literal::Float(i)),
            Some(Token::STRING_LITERAL(s)) => Ok(Literal::String(s)),
            _                              => Err(String::from("Literal Parse Error"))
        };
    }

    /// <pre>
    ///     grouping :: LPAREN expr RPAREN
    /// </pre>
    fn grouping(&mut self) -> Result<Expr, String> {
        return match (self.lexer.next(), self.expr()?, self.lexer.next()) {
            (Some(Token::LPAREN), expr, Some(Token::RPAREN)) => Ok(expr),
            _                                                => Err(String::from("Grouping Parse Error"))
        };
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

