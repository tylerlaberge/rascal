use lexer::Lexer;
use lexer::Token;
use super::ast::Program;
use super::ast::Block;
use super::ast::Declarations;
use super::ast::VariableDeclaration;
use super::ast::TypeSpec;
use super::ast::Compound;
use super::ast::StatementList;
use super::ast::Statement;
use super::ast::Assignment;
use super::ast::Variable;
use super::ast::Expr;
use super::ast::Operator;

/// <pre>
///     program              :: PROGRAM variable SEMI block DOT
///     block                :: declarations compound_statement
///     declarations         :: VAR (variable_declaration SEMI)+ | empty
///     variable_declaration :: ID (COMMA ID)* COLON type_spec
///     type_spec            :: INTEGER | REAL
///     compound_statement   :: BEGIN statement_list END
///     statement_list       :: statement | statement SEMI statement_list
///     statement            :: compound_statement | assignment_statement | empty
///     assignment_statement :: variable ASSIGN expr
///     variable             :: ID
///     expr                 :: term ((PLUS | MINUS) term)*
///     term                 :: factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*
///     factor               :: (PLUS | MINUS) factor | INTEGER_CONST | REAL_CONST | LPAREN expr RPAREN | variable
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
    ///     declarations :: VAR (variable_declaration SEMI)+ | empty
    /// </pre>
    fn declarations(&mut self) -> Result<Declarations, String> {
        if let Some(&Token::VAR) = self.lexer.peek() {
            self.lexer.next(); // eat the var

            let mut variable_declarations: Vec<VariableDeclaration> = vec![];
            while let Some(&Token::ID(_)) = self.lexer.peek() {
                match (self.variable_declaration()?, self.lexer.next()) {
                    (variable_declaration, Some(Token::SEMI)) => {
                        variable_declarations.push(variable_declaration);
                        Ok(())
                    },
                    _                                         => Err(String::from("Declarations Parse Error"))
                }?;
            }

            if variable_declarations.len() > 0 {
                return Ok(Declarations::VariableDeclarations(variable_declarations));
            }
            else {
                return Err(String::from("Declarations Parse Error"));
            }
        }
        else {
            return Ok(Declarations::Empty);
        }
    }

    /// <pre>
    ///     variable_declaration :: ID (COMMA ID)* COLON type_spec
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

        return match (self.lexer.next(), self.type_spec()?) {
            (Some(Token::COLON), type_spec) => Ok(VariableDeclaration::Variables(ids, type_spec)),
            _                               => Err(String::from("Variable Declaration Parse Error"))
        };
    }
    /// <pre>
    ///     type_spec:: INTEGER | REAL
    /// </pre>
    fn type_spec(&mut self) -> Result<TypeSpec, String> {
        return match self.lexer.next() {
            Some(Token::INTEGER) => Ok(TypeSpec::INTEGER),
            Some(Token::REAL)    => Ok(TypeSpec::REAL),
            _                    => Err(String::from("TypeSpec Parse Error"))
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
                Some(&Token::SEMI) => self.lexer.next(),
                _                  => break
            };
        }

        return Ok(StatementList::Statements(statements));
    }

    /// <pre>
    ///     statement :: compound_statement | assignment_statement | empty
    /// </pre>
    fn statement(&mut self) -> Result<Statement, String> {
        return match self.lexer.peek() {
            Some(&Token::BEGIN) => Ok(Statement::Compound(self.compound_statement()?)),
            Some(&Token::ID(_)) => Ok(Statement::Assignment(self.assignment_statement()?)),
            Some(&Token::END)   => Ok(Statement::Empty),
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
    ///     expr :: term ((PLUS | MINUS) term)*
    /// </pre>
    fn expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.term()?;

        while matches!(self.lexer.peek(), Some(&Token::PLUS) | Some(&Token::MINUS)) {
            expr = match (self.lexer.next(), self.term()) {
                (Some(Token::PLUS), Ok(term))  => Ok(Expr::BinOp(Box::new(expr), Operator::Plus, Box::new(term))),
                (Some(Token::MINUS), Ok(term)) => Ok(Expr::BinOp(Box::new(expr), Operator::Minus, Box::new(term))),
                _                              => Err(String::from("Expression Parse Error"))
            }?;
        }

        return Ok(expr);
    }

    /// <pre>
    ///     term :: factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*
    /// </pre>
    fn term(&mut self) -> Result<Expr, String> {
        let mut term = self.factor()?;

        while matches!(self.lexer.peek(), Some(&Token::MULTIPLY) | Some(&Token::INTEGER_DIV) | Some(&Token::FLOAT_DIV)) {
            term = match (self.lexer.next(), self.factor()) {
                (Some(Token::MULTIPLY), Ok(factor))    => Ok(Expr::BinOp(Box::new(term), Operator::Multiply, Box::new(factor))),
                (Some(Token::INTEGER_DIV), Ok(factor)) => Ok(Expr::BinOp(Box::new(term), Operator::IntegerDivide, Box::new(factor))),
                (Some(Token::FLOAT_DIV), Ok(factor))   => Ok(Expr::BinOp(Box::new(term), Operator::FloatDivide, Box::new(factor))),
                _                                      => Err(String::from("Term Parse Error"))
            }?;
        }

        return Ok(term);
    }

    /// <pre>
    ///     factor :: (PLUS | MINUS) factor | INTEGER_CONST | REAL_CONST | LPAREN expr RPAREN | variable
    /// </pre>
    fn factor(&mut self) -> Result<Expr, String> {
        if let Some(&Token::ID(_)) = self.lexer.peek() {
            return Ok(Expr::Variable(self.variable()?));
        }
        else {
            return match self.lexer.next() {
                Some(Token::PLUS)             => Ok(Expr::UnaryOp(Operator::Plus, Box::new(self.factor()?))),
                Some(Token::MINUS)            => Ok(Expr::UnaryOp(Operator::Minus, Box::new(self.factor()?))),
                Some(Token::INTEGER_CONST(i)) => Ok(Expr::Int(i)),
                Some(Token::REAL_CONST(i))    => Ok(Expr::Float(i)),
                Some(Token::LPAREN)           =>
                    match (self.expr(), self.lexer.next()) {
                        (Ok(expr), Some(Token::RPAREN)) => Ok(expr),
                        _                               => Err(String::from("Factor Parse Error"))
                    },
                _                             => Err(String::from("Parse Error"))
            };
        }
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

