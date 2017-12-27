#[derive(Debug, Clone, PartialEq)]
pub struct Program(pub Variable, pub Block);

#[derive(Debug, Clone, PartialEq)]
pub struct Block(pub Vec<Declarations>, pub Compound);

#[derive(Debug, Clone, PartialEq)]
pub enum Declarations {
    ProcedureDeclarations(Vec<ProcedureDeclaration>),
    FunctionDeclarations(Vec<FunctionDeclaration>),
    VariableDeclarations(Vec<VariableDeclaration>),
    Empty
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcedureDeclaration(pub String, pub FormalParameterList, pub Block);

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration(pub String, pub FormalParameterList, pub Block, pub TypeSpec);

#[derive(Debug, Clone, PartialEq)]
pub struct FormalParameterList(pub Vec<FormalParameters>);

#[derive(Debug, Clone, PartialEq)]
pub struct FormalParameters(pub Vec<String>, pub TypeSpec);

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration(pub Vec<String>, pub TypeSpec);

#[derive(Debug, Clone, PartialEq)]
pub enum TypeSpec {
    INTEGER,
    REAL,
    STRING,
    BOOLEAN,
    UNIT
}

#[derive(Debug, Clone, PartialEq)]
pub struct Compound(pub Vec<Statement>);

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Compound(Compound),
    Assignment(Assignment),
    IfStatement(IfStatement),
    FunctionCall(FunctionCall),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Assignment(pub Variable, pub Expr);

#[derive(Debug, Clone, PartialEq)]
pub enum IfStatement {
    If(Expr, Compound),
    IfElse(Expr, Compound, Compound),
    IfElseIf(Expr, Compound, Box<IfStatement>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable(pub String);

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall(pub Variable, pub CallParameters);

#[derive(Debug, Clone, PartialEq)]
pub struct CallParameters(pub Vec<Expr>);

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    UnaryOp(Box<UnaryOpExpr>),
    BinOp(Box<BinaryOpExpr>),
    Group(Box<GroupedExpr>),
    FunctionCall(FunctionCall),
    Literal(Literal),
    Variable(Variable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOpExpr(pub UnaryOperator, pub Expr);

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Not
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOpExpr(pub Expr, pub BinaryOperator, pub Expr);

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    IntegerDivide,
    FloatDivide,
    And,
    Or,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual
}

#[derive(Debug, Clone, PartialEq)]
pub struct GroupedExpr(pub Expr);

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i32),
    Float(f32),
    String(String),
    Boolean(bool)
}