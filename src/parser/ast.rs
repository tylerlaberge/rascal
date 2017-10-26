#[derive(Debug)]
pub enum AST {
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    Num(u32),
    BinOp(Box<Expr>, Operator, Box<Expr>),
    UnaryOp(Operator, Box<Expr>),
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide
}