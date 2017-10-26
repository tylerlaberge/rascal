use parser::ast::AST;
use parser::ast::Expr;

pub trait NodeVisitor {
    fn visit_unaryop(&mut self, node: Expr) -> Result<i32, String>;
    fn visit_binop(&mut self, node: Expr) -> Result<i32, String>;
    fn visit_num(&mut self, node: Expr) -> Result<i32, String>;
    fn visit_expr(&mut self, node: Expr) -> Result<i32, String> {
        return match node {
            Expr::UnaryOp(_, _)  => self.visit_unaryop(node),
            Expr::BinOp(_, _, _) => self.visit_binop(node),
            Expr::Num(_)         => self.visit_num(node),
        }
    }

    fn visit(&mut self, ast: AST) -> Result<(), String> {
        match ast {
            AST::Expr(expr) => println!("{}", self.visit_expr(expr)?),
        }

        return Ok(());
    }
}