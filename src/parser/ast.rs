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
    FunctionDeclarations(Vec<FunctionDeclaration>),
    VariableDeclarations(Vec<VariableDeclaration>),
    Empty
}

#[derive(Debug, Clone)]
pub enum ProcedureDeclaration {
    Procedure(String, FormalParameterList, Block)
}

#[derive(Debug, Clone)]
pub enum FunctionDeclaration {
    Function(String, FormalParameterList, Block, TypeSpec)
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
    STRING,
    UNIT
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
    FunctionCall(FunctionCall),
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
pub enum FunctionCall {
    Call(Variable, CallParameters)
}

#[derive(Debug, Clone)]
pub enum CallParameters {
    Parameters(Vec<Expr>)
}

#[derive(Debug, Clone)]
pub enum Expr {
    UnaryOp(Box<UnaryOpExpr>),
    BinOp(Box<BinaryOpExpr>),
    Group(Box<GroupedExpr>),
    FunctionCall(FunctionCall),
    Literal(Literal),
    Variable(Variable),
}

#[derive(Debug, Clone)]
pub enum UnaryOpExpr {
    UnaryOp(UnaryOperator, Expr)
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Plus,
    Minus
}

#[derive(Debug, Clone)]
pub enum BinaryOpExpr {
    BinaryOp(Expr, BinaryOperator, Expr)
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    IntegerDivide,
    FloatDivide
}

#[derive(Debug, Clone)]
pub enum GroupedExpr {
    Group(Expr)
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i32),
    Float(f32),
    String(String)
}