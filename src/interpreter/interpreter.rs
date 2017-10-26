use parser::ast::Expr;
use parser::ast::Operator;
use parser::Parser;
use super::node_visitor::NodeVisitor;

pub struct Interpreter<'a> {
    parser: Parser<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn new(parser: Parser<'a>) -> Interpreter<'a> {
        return Interpreter { parser };
    }

    pub fn interpret(&mut self) -> Result<(), String> {
        return match self.parser.parse() {
            Ok(ast) => self.visit(ast),
            Err(e)  => Err(e)
        }
    }
}

impl<'a> NodeVisitor for Interpreter<'a> {

    fn visit_unaryop(&mut self, expr: Expr) -> Result<i32, String> {
        return match expr {
            Expr::UnaryOp(Operator::Plus, factor)  => Ok(self.visit_expr(*factor)?),
            Expr::UnaryOp(Operator::Minus, factor) => Ok(-self.visit_expr(*factor)?),
            _                                      => Err(String::from("Interpreter Error"))
        }
    }

    fn visit_binop(&mut self, expr: Expr) -> Result<i32, String> {
        return match expr {
            Expr::BinOp(left, Operator::Plus, right)     => Ok(self.visit_expr(*left)? + self.visit_expr(*right)?),
            Expr::BinOp(left, Operator::Minus, right)    => Ok(self.visit_expr(*left)? - self.visit_expr(*right)?),
            Expr::BinOp(left, Operator::Multiply, right) => Ok(self.visit_expr(*left)? * self.visit_expr(*right)?),
            Expr::BinOp(left, Operator::Divide, right)   => Ok(self.visit_expr(*left)? / self.visit_expr(*right)?),
            _                                            => Err(String::from("Interpreter Error"))
        }
    }

    fn visit_num(&mut self, expr: Expr) -> Result<i32, String> {
        return match expr {
            Expr::Num(i) => Ok(i as i32),
            _            => Err(String::from("Interpreter Error"))
        }
    }
}
