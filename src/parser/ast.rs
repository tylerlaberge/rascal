#[derive(Debug)]
pub enum Program {
    Compound(Compound)
}

#[derive(Debug)]
pub enum Compound {
    StatementList(StatementList)
}

#[derive(Debug)]
pub enum StatementList {
    Statements(Vec<Statement>)
}

#[derive(Debug)]
pub enum Statement {
    Compound(Compound),
    Assignment(Assignment),
    NoOp
}

#[derive(Debug)]
pub enum Assignment {
    Assign(Variable, Expr)
}

#[derive(Debug)]
pub enum Variable {
    Var(String)
}

#[derive(Debug)]
pub enum Expr {
    Num(u32),
    BinOp(Box<Expr>, Operator, Box<Expr>),
    UnaryOp(Operator, Box<Expr>),
    Variable(Variable)
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide
}