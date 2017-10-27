use lexer::Lexer;
use lexer::Token;
use super::ast::Program;
use super::ast::Compound;
use super::ast::StatementList;
use super::ast::Statement;
use super::ast::Assignment;
use super::ast::Variable;
use super::ast::Expr;
use super::ast::Operator;

/// <pre>
///     program              :: compound_statement DOT
///     compound_statement   :: BEGIN statement_list END
///     statement_list       :: statement | statement SEMI statement_list
///     statement            :: compound_statement | assignment_statement | empty
///     assignment_statement :: variable ASSIGN expr
///     variable             :: ID
///     expr                 :: term ((PLUS | MINUS) term)*
///     term                 :: factor ((MUL | DIV) factor)*
///     factor               :: (PLUS | MINUS) factor | INTEGER | LPAREN expr RPAREN | variable
/// </pre>
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {

    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        return Parser { lexer };
    }

    /// <pre>
    ///     program :: compound_statement DOT
    /// </pre>
    fn program(&mut self) -> Result<Program, String> {
        let compound = self.compound_statement()?;

        return match self.lexer.next() {
            Some(Token::DOT) => Ok(Program::Compound(compound)),
            _                => Err(String::from("Program Parse Error"))
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
            Some(&Token::END)   => Ok(Statement::NoOp),
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
    ///     term :: factor ((MUL | DIV) factor)*
    /// </pre>
    fn term(&mut self) -> Result<Expr, String> {
        let mut term = self.factor()?;

        while matches!(self.lexer.peek(), Some(&Token::MULTIPLY) | Some(&Token::DIVIDE)) {
            term = match (self.lexer.next(), self.factor()) {
                (Some(Token::MULTIPLY), Ok(factor)) => Ok(Expr::BinOp(Box::new(term), Operator::Multiply, Box::new(factor))),
                (Some(Token::DIVIDE), Ok(factor))   => Ok(Expr::BinOp(Box::new(term), Operator::Divide, Box::new(factor))),
                _                                   => Err(String::from("Term Parse Error"))
            }?;
        }

        return Ok(term);
    }

    /// <pre>
    ///     factor :: (PLUS | MINUS) factor | INTEGER | LPAREN expr RPAREN | variable
    /// </pre>
    fn factor(&mut self) -> Result<Expr, String> {
        if let Some(&Token::ID(_)) = self.lexer.peek() {
            return Ok(Expr::Variable(self.variable()?));
        }
        else {
            return match self.lexer.next() {
                Some(Token::PLUS)       => Ok(Expr::UnaryOp(Operator::Plus, Box::new(self.factor()?))),
                Some(Token::MINUS)      => Ok(Expr::UnaryOp(Operator::Minus, Box::new(self.factor()?))),
                Some(Token::INTEGER(i)) => Ok(Expr::Num(i)),
                Some(Token::LPAREN)     =>
                    match (self.expr(), self.lexer.next()) {
                        (Ok(expr), Some(Token::RPAREN)) => Ok(expr),
                        _                               => Err(String::from("Factor Parse Error"))
                    },
                _                       => Err(String::from("Parse Error"))
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

