use lexer::Lexer;
use lexer::Token;
use super::ast::AST;
use super::ast::Expr;
use super::ast::Operator;

/// <pre>
///     expr   :: term ((PLUS | MINUS) term)*
///     term   :: factor ((MUL | DIV) factor)*
///     factor :: (PLUS | MINUS) factor | INTEGER | LPAREN expr RPAREN
/// </pre>
pub struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {

    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        return Parser { lexer };
    }

    /// <pre>
    ///     expr   :: term ((PLUS | MINUS) term)*
    /// </pre>
    fn expr(&mut self) -> Result<Expr, String> {
        let mut expr = self.term()?;

        while matches!(self.lexer.peek(), Some(&Token::PLUS) | Some(&Token::MINUS)) {
            expr = match (self.lexer.next(), self.term()) {
                (Some(Token::PLUS), Ok(term))  => Ok(Expr::BinOp(Box::new(expr), Operator::Plus, Box::new(term))),
                (Some(Token::MINUS), Ok(term)) => Ok(Expr::BinOp(Box::new(expr), Operator::Minus, Box::new(term))),
                _                              => Err(String::from("Parse Error"))
            }?;
        }

        return Ok(expr);
    }

    /// <pre>
    ///     term   :: factor ((MUL | DIV) factor)*
    /// </pre>
    fn term(&mut self) -> Result<Expr, String> {
        let mut term = self.factor()?;

        while matches!(self.lexer.peek(), Some(&Token::MULTIPLY) | Some(&Token::DIVIDE)) {
            term = match (self.lexer.next(), self.factor()) {
                (Some(Token::MULTIPLY), Ok(factor)) => Ok(Expr::BinOp(Box::new(term), Operator::Multiply, Box::new(factor))),
                (Some(Token::DIVIDE), Ok(factor))   => Ok(Expr::BinOp(Box::new(term), Operator::Divide, Box::new(factor))),
                _                                   => Err(String::from("Parse Error"))
            }?;
        }

        return Ok(term);
    }

    /// <pre>
    ///     factor :: (PLUS | MINUS) factor | INTEGER | LPAREN expr RPAREN
    /// </pre>
    fn factor(&mut self) -> Result<Expr, String> {
        return match self.lexer.next() {
            Some(Token::PLUS)       => Ok(Expr::UnaryOp(Operator::Plus, Box::new(self.factor()?))),
            Some(Token::MINUS)      => Ok(Expr::UnaryOp(Operator::Minus, Box::new(self.factor()?))),
            Some(Token::INTEGER(i)) => Ok(Expr::Num(i)),
            Some(Token::LPAREN)     =>
                match (self.expr(), self.lexer.next()) {
                    (Ok(expr), Some(Token::RPAREN)) => Ok(expr),
                    _                               => Err(String::from("Parse Error"))
                },
            _                       => Err(String::from("Parse Error"))
        };
    }

    pub fn parse(&mut self) -> Result<AST, String> {
        let expr = self.expr();

        match self.lexer.next() {
            Some(Token::EOF) => Ok(()),
            Some(token)      => Err(format!("Parse Error: Expected EOF token, found '{:?}'", token)),
            None             => Err(String::from("Internal Parse Error"))
        }?;

        return Ok(AST::Expr(expr?));
    }
}

