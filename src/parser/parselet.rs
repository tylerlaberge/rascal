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
            _                             => Err(String::from("Literal Parse Error: Expected integer, real, string, or boolean const"))
        };
    }

    fn grouping(&self, parser: &mut Parser, token: &Token) -> Result<GroupedExpr, String> {
        return match token {
            &Token::LPAREN => match (parser.expr(None)?, parser.lexer.next()) {
                (expr, Some(Token::RPAREN)) => Ok(GroupedExpr::Group(expr)),
                (expr, _)                   => Err(String::from(format!("Grouping Parse Error: Expected token {:?} after {:?}", Token::RPAREN, expr)))
            },
            _              => Err(String::from(format!("Grouping Parse Error: Expected token {:?}", Token::LPAREN)))
        };
    }

    fn variable(&self, parser: &mut Parser, token: &Token) -> Result<Variable, String> {
        return match token {
            &Token::ID(ref name) => Ok(Variable::Var(name.clone())),
            _                    => Err(String::from("Variable Parse Error: Expected token id"))
        };
    }

    fn unary_op(&self, parser: &mut Parser, token: &Token) -> Result<UnaryOpExpr, String> {
        return match token {
            &Token::PLUS  => Ok(UnaryOpExpr::UnaryOp(UnaryOperator::Plus, parser.expr(Some(self.get_precedence()))?)),
            &Token::MINUS => Ok(UnaryOpExpr::UnaryOp(UnaryOperator::Minus, parser.expr(Some(self.get_precedence()))?)),
            &Token::NOT   => Ok(UnaryOpExpr::UnaryOp(UnaryOperator::Not, parser.expr(Some(self.get_precedence()))?)),
            _             => Err(String::from(format!("Unary Op Parse Error: Expected one of {:?}", vec![Token::PLUS, Token::MINUS, Token::NOT])))

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
        return match token {
            &Token::PLUS                  => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::Plus,              parser.expr(Some(self.get_precedence()))?)),
            &Token::MINUS                 => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::Minus,             parser.expr(Some(self.get_precedence()))?)),
            &Token::MULTIPLY              => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::Multiply,          parser.expr(Some(self.get_precedence()))?)),
            &Token::INTEGER_DIV           => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::IntegerDivide,     parser.expr(Some(self.get_precedence()))?)),
            &Token::FLOAT_DIV             => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::FloatDivide,       parser.expr(Some(self.get_precedence()))?)),
            &Token::AND                   => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::And,               parser.expr(Some(self.get_precedence()))?)),
            &Token::OR                    => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::Or,                parser.expr(Some(self.get_precedence()))?)),
            &Token::LESS_THAN             => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::LessThan,          parser.expr(Some(self.get_precedence()))?)),
            &Token::LESS_THAN_OR_EQUAL    => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::LessThanOrEqual,   parser.expr(Some(self.get_precedence()))?)),
            &Token::GREATER_THAN          => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::GreaterThan,       parser.expr(Some(self.get_precedence()))?)),
            &Token::GREATER_THAN_OR_EQUAL => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::GreaterThanOrEqual,parser.expr(Some(self.get_precedence()))?)),
            &Token::EQUAL                 => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::Equal,             parser.expr(Some(self.get_precedence()))?)),
            &Token::NOT_EQUAL             => Ok(BinaryOpExpr::BinaryOp(left.clone(), BinaryOperator::NotEqual,          parser.expr(Some(self.get_precedence()))?)),
            _                             => Err(
                String::from(format!("Binary Op Parse Error: Expected one of {:?}", vec![
                    Token::PLUS, Token::MINUS, Token::MULTIPLY, Token::INTEGER_DIV, Token::FLOAT_DIV, Token::AND,
                    Token::OR, Token::LESS_THAN, Token::LESS_THAN_OR_EQUAL, Token::GREATER_THAN, Token::GREATER_THAN_OR_EQUAL,
                    Token::EQUAL, Token::NOT_EQUAL
                ]))
            )
        };
    }

    fn function_call(&self, parser: &mut Parser, left: &Expr, token: &Token) -> Result<FunctionCall, String> {
        return match (left, token) {
            (&Expr::Variable(ref name), &Token::LPAREN) => match (parser.call_parameters()?, parser.lexer.next()) {
                (parameters, Some(Token::RPAREN)) => Ok(FunctionCall::Call(name.clone(), parameters)),
                (parameters, _)                   => Err(String::from(format!("Function Call Parse Error: Expected token {:?} after {:?}", Token::RPAREN, parameters)))
            },
            (_, _)                                      => Err(String::from(format!("Function Call Parse Error: Expected variable token and {:?}", Token::LPAREN)))
        };
    }
}