use lexer::Token;
use super::parser::Parser;
use super::ast::{
    Expr,
    UnaryOpExpr,
    UnaryOperator,
    BinaryOpExpr,
    BinaryOperator,
    GroupedExpr,
    FunctionCall,
    Variable,
    Literal
};

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
        let operator = match token {
            &Token::PLUS     => Ok(UnaryOperator::Plus),
            &Token::MINUS    => Ok(UnaryOperator::Minus),
            &Token::NOT      => Ok(UnaryOperator::Not),
            _                => Err(String::from(format!("Unary Op Parse Error: Expected one of {:?}", vec![Token::PLUS, Token::MINUS, Token::NOT])))
        }?;

        return Ok(UnaryOpExpr::UnaryOp(operator, parser.expr(Some(self.get_precedence()))?));
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
        let operator = match token {
            &Token::PLUS                  => Ok(BinaryOperator::Plus),
            &Token::MINUS                 => Ok(BinaryOperator::Minus),
            &Token::MULTIPLY              => Ok(BinaryOperator::Multiply),
            &Token::INTEGER_DIV           => Ok(BinaryOperator::IntegerDivide),
            &Token::FLOAT_DIV             => Ok(BinaryOperator::FloatDivide),
            &Token::AND                   => Ok(BinaryOperator::And),
            &Token::OR                    => Ok(BinaryOperator::Or),
            &Token::LESS_THAN             => Ok(BinaryOperator::LessThan),
            &Token::LESS_THAN_OR_EQUAL    => Ok(BinaryOperator::LessThanOrEqual),
            &Token::GREATER_THAN          => Ok(BinaryOperator::GreaterThan),
            &Token::GREATER_THAN_OR_EQUAL => Ok(BinaryOperator::GreaterThanOrEqual),
            &Token::EQUAL                 => Ok(BinaryOperator::Equal),
            &Token::NOT_EQUAL             => Ok(BinaryOperator::NotEqual),
            _                             => Err(
                String::from(format!("Binary Op Parse Error: Expected one of {:?}", vec![
                    Token::PLUS, Token::MINUS, Token::MULTIPLY, Token::INTEGER_DIV, Token::FLOAT_DIV, Token::AND,
                    Token::OR, Token::LESS_THAN, Token::LESS_THAN_OR_EQUAL, Token::GREATER_THAN, Token::GREATER_THAN_OR_EQUAL,
                    Token::EQUAL, Token::NOT_EQUAL
                ]))
            )
        }?;

        return Ok(BinaryOpExpr::BinaryOp(left.clone(), operator, parser.expr(Some(self.get_precedence()))?));
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