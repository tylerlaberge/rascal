#[derive(Debug)]
pub enum Program {
    Program(Variable, Block)
}

#[derive(Debug)]
pub enum Block {
    Block(Declarations, Compound)
}

#[derive(Debug)]
pub enum Declarations {
    VariableDeclarations(Vec<VariableDeclaration>),
    Empty
}

#[derive(Debug)]
pub enum VariableDeclaration {
    Variables(Vec<String>, TypeSpec)
}

#[derive(Debug)]
pub enum TypeSpec {
    INTEGER,
    REAL
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
    Empty
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
    Num(f32),
    BinOp(Box<Expr>, Operator, Box<Expr>),
    UnaryOp(Operator, Box<Expr>),
    Variable(Variable)
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    IntegerDivide,
    FloatDivide
}