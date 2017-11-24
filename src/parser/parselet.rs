use lexer::Token;

use super::ast::Expr;
use super::ast::UnaryOpExpr;
use super::ast::UnaryOperator;
use super::ast::BinaryOpExpr;
use super::ast::BinaryOperator;
use super::ast::GroupedExpr;
use super::ast::FunctionCall;
use super::ast::Variable;
use super::ast::Literal;

use super::parser::Parser;

pub enum PrefixParselet {
    Literal,
    Grouping,
    Variable,
    UnaryOperator(u32)
}
pub enum InfixParselet {
    FunctionCall(u32),
    BinaryOperator(u32)
}

#[allow(unused_variables)]
impl PrefixParselet {

    pub fn parse(&self, parser: &mut Parser, token: &Token) -> Result<Expr, String> {
        return match self {
            &PrefixParselet::Literal          => Ok(Expr::Literal(self.literal(parser, token)?)),
            &PrefixParselet::Grouping         => Ok(Expr::Group(Box::new(self.grouping(parser, token)?))),
            &PrefixParselet::Variable         => Ok(Expr::Variable(self.variable(parser, token)?)),
            &PrefixParselet::UnaryOperator(_) => Ok(Expr::UnaryOp(Box::new(self.unary_op(parser, token)?)))
        }
    }

    pub fn get_precedence(&self) -> u32 {
        return match self {
            &PrefixParselet::UnaryOperator(precedence) => precedence,
            _                                          => 0
        };
    }

    fn literal(&self, parser: &mut Parser, token: &Token) -> Result<Literal, String> {
        return match token {
            &Token::INTEGER_CONST(i)      => Ok(Literal::Int(i)),
            &Token::REAL_CONST(i)         => Ok(Literal::Float(i)),
            &Token::STRING_LITERAL(ref s) => Ok(Literal::String(s.clone())),
            &Token::BOOLEAN_CONST(b)      => Ok(Literal::Boolean(b)),
            _                             => Err(String::from("Literal Parse Error"))
        };
    }

    fn grouping(&self, parser: &mut Parser, token: &Token) -> Result<GroupedExpr, String> {
        return match (token, parser.expr(None)?, parser.lexer.next()) {
            (&Token::LPAREN, expr, Some(Token::RPAREN)) => Ok(GroupedExpr::Group(expr)),
            _                                           => Err(String::from("Grouping Parse Error"))
        };
    }

    fn variable(&self, parser: &mut Parser, token: &Token) -> Result<Variable, String> {
        return match token {
            &Token::ID(ref name) => Ok(Variable::Var(name.clone())),
            _                    => Err(String::from("Variable Parse Error"))
        };
    }

    fn unary_op(&self, parser: &mut Parser, token: &Token) -> Result<UnaryOpExpr, String> {
        return match (token, parser.expr(Some(self.get_precedence()))?) {
            (&Token::PLUS, operand)  => Ok(UnaryOpExpr::UnaryOp(UnaryOperator::Plus, operand)),
            (&Token::MINUS, operand) => Ok(UnaryOpExpr::UnaryOp(UnaryOperator::Minus, operand)),
            (&Token::NOT, operand)   => Ok(UnaryOpExpr::UnaryOp(UnaryOperator::Not, operand)),
            _                        => Err(String::from("Unary Op Parse Error"))
        };
    }
}

#[allow(unused_variables)]
impl InfixParselet {
    pub fn parse(&self, parser: &mut Parser, left: &Expr, token: &Token) -> Result<Expr, String> {
        return match self {
            &InfixParselet::BinaryOperator(_) => Ok(Expr::BinOp(Box::new(self.binary_op(parser, left, token)?))),
            &InfixParselet::FunctionCall(_)   => Ok(Expr::FunctionCall(self.function_call(parser, left, token)?)),
        };
    }

    pub fn get_precedence(&self) -> u32 {
        return match self {
            &InfixParselet::BinaryOperator(precedence)
            | &InfixParselet::FunctionCall(precedence) => precedence
        };
    }

    fn binary_op(&self, parser: &mut Parser, left: &Expr, token: &Token) -> Result<BinaryOpExpr, String> {
        return match (token, parser.expr(Some(self.get_precedence()))?) {
            (&Token::PLUS, right)                  => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::Plus, right)),
            (&Token::MINUS, right)                 => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::Minus, right)),
            (&Token::MULTIPLY, right)              => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::Multiply, right)),
            (&Token::INTEGER_DIV, right)           => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::IntegerDivide, right)),
            (&Token::FLOAT_DIV, right)             => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::FloatDivide, right)),
            (&Token::AND, right)                   => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::And, right)),
            (&Token::OR, right)                    => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::Or, right)),
            (&Token::LESS_THAN, right)             => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::LessThan, right)),
            (&Token::LESS_THAN_OR_EQUAL, right)    => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::LessThanOrEqual, right)),
            (&Token::GREATER_THAN, right)          => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::GreaterThan, right)),
            (&Token::GREATER_THAN_OR_EQUAL, right) => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::GreaterThanOrEqual, right)),
            (&Token::EQUAL, right)                 => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::Equal, right)),
            (&Token::NOT_EQUAL, right)             => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::NotEqual, right)),

            _                                      => Err(String::from("Binary Op Parse Error"))
        };
    }

    fn function_call(&self, parser: &mut Parser, left: &Expr, token: &Token) -> Result<FunctionCall, String> {
        return match (left, token, parser.call_parameters()?, parser.lexer.next()) {
            (&Expr::Variable(ref name), &Token::LPAREN, ref parameters, Some(Token::RPAREN)) => Ok(FunctionCall::Call(name.clone(), parameters.clone())),
            _                                                                                => Err(String::from("Function Call Parse Error"))
        };
    }
}