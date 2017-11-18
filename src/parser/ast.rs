#[derive(Debug, Clone)]
pub enum Program {
    Program(Variable, Block)
}

#[derive(Debug, Clone)]
pub enum Block {
    Block(Vec<Declarations>, Compound)
}

#[derive(Debug, Clone)]
pub enum Declarations {
    ProcedureDeclarations(Vec<ProcedureDeclaration>),
    VariableDeclarations(Vec<VariableDeclaration>),
    Empty
}

#[derive(Debug, Clone)]
pub enum ProcedureDeclaration {
    Procedure(String, FormalParameterList, Block)
}

#[derive(Debug, Clone)]
pub enum FormalParameterList {
    FormalParameters(Vec<FormalParameters>)
}

#[derive(Debug, Clone)]
pub enum FormalParameters {
    Parameters(Vec<String>, TypeSpec)
}

#[derive(Debug, Clone)]
pub enum VariableDeclaration {
    Variables(Vec<String>, TypeSpec)
}

#[derive(Debug, Clone)]
pub enum TypeSpec {
    INTEGER,
    REAL,
    STRING
}

#[derive(Debug, Clone)]
pub enum Compound {
    StatementList(StatementList)
}

#[derive(Debug, Clone)]
pub enum StatementList {
    Statements(Vec<Statement>)
}

#[derive(Debug, Clone)]
pub enum Statement {
    Compound(Compound),
    Assignment(Assignment),
    ProcedureCall(ProcedureCall),
    Empty
}

#[derive(Debug, Clone)]
pub enum Assignment {
    Assign(Variable, Expr)
}

#[derive(Debug, Clone)]
pub enum Variable {
    Var(String)
}

#[derive(Debug, Clone)]
pub enum ProcedureCall {
    Call(Variable, ProcedureParameters)
}

#[derive(Debug, Clone)]
pub enum ProcedureParameters {
    Parameters(Vec<Expr>)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Int(i32),
    Float(f32),
    String(String),
    BinOp(Box<Expr>, Operator, Box<Expr>),
    UnaryOp(Operator, Box<Expr>),
    Variable(Variable)
}

#[derive(Debug, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    IntegerDivide,
    FloatDivide
}