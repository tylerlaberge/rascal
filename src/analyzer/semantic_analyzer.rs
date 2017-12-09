use parser::ast::Program;
use parser::ast::Block;
use parser::ast::Declarations;
use parser::ast::ProcedureDeclaration;
use parser::ast::FormalParameterList;
use parser::ast::FormalParameters;
use parser::ast::VariableDeclaration;
use parser::ast::FunctionDeclaration;
use parser::ast::Compound;
use parser::ast::Statement;
use parser::ast::IfStatement;
use parser::ast::TypeSpec;
use parser::ast::FunctionCall;
use parser::ast::CallParameters;
use parser::ast::Expr;
use parser::ast::BinaryOpExpr;
use parser::ast::BinaryOperator;
use parser::ast::UnaryOpExpr;
use parser::ast::UnaryOperator;
use parser::ast::GroupedExpr;
use parser::ast::Literal;
use parser::ast::Assignment;
use parser::ast::Variable;

use super::symbol_table::SymbolTable;
use super::symbol::Symbol;
use super::symbol::VarSymbol;
use super::symbol::CallableSymbol;
use super::built_ins;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt;

pub struct SemanticAnalyzer {
    scope: Option<SymbolTable>
}

impl Display for SemanticAnalyzer {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        return write!(f, "{:?}", self.scope);
    }
}

impl SemanticAnalyzer {

    pub fn new() -> SemanticAnalyzer {
        return SemanticAnalyzer { scope: None };
    }

    pub fn analyze(&mut self, program: &Program) {
        match self.visit_program(program) {
            Ok(()) => (),
            Err(e) => panic!("{}", e)
        }
    }

    fn init_built_ins(&mut self) -> Result<(), String> {
        self.scope()?.define(built_ins::write_procedure());
        self.scope()?.define(built_ins::writeln_procedure());
        self.scope()?.define(built_ins::readln_procedure());
        self.scope()?.define(built_ins::int_to_string_function());
        self.scope()?.define(built_ins::real_to_string_function());
        self.scope()?.define(built_ins::string_to_int_function());
        self.scope()?.define(built_ins::string_to_real_function());

        return Ok(());
    }

    pub fn visit_program(&mut self, node: &Program) -> Result<(), String> {
        return match node {
            &Program::Program(ref var, ref block) => {
                match var {
                    &Variable::Var(ref name) => {
                        self.enter_scope(name.to_owned());
                        self.init_built_ins()?;
                    }
                };

                self.visit_block(block)?;
                self.leave_scope();

                Ok(())
            }
        };
    }

    pub fn visit_block(&mut self, node: &Block) -> Result<TypeSpec, String> {
        return match node {
            &Block::Block(ref declarations, ref compound) => {
                self.visit_declarations(declarations)?;
                let return_type = self.visit_compound(compound)?;

                Ok(return_type)
            }
        }
    }

    pub fn visit_declarations(&mut self, node: &Vec<Declarations>) -> Result<(), String> {
        for declarations in node {
            match declarations {
                &Declarations::ProcedureDeclarations(ref procedure_declarations) => {
                    for procedure_declaration in procedure_declarations {
                        self.visit_procedure_declaration(procedure_declaration)?;
                    }
                },
                &Declarations::FunctionDeclarations(ref function_declarations) => {
                    for function_declaration in function_declarations {
                        self.visit_function_declaration(function_declaration)?;
                    }
                },
                &Declarations::VariableDeclarations(ref variable_declarations)   => {
                    for variable_declaration in variable_declarations {
                        self.visit_variable_declaration(variable_declaration)?;
                    }
                },
                &Declarations::Empty                                             => ()
            };
        }
        return Ok(());
    }

    pub fn visit_procedure_declaration(&mut self, node: &ProcedureDeclaration) -> Result<(), String> {
        return match node {
            &ProcedureDeclaration::Procedure(ref name, ref parameter_list, ref block) => {
                let parameters = self.visit_formal_parameter_list(parameter_list)?;

                self.scope()?.define(Symbol::Callable(CallableSymbol::Procedure(name.to_owned(), parameters.to_vec())));
                self.enter_scope(name.to_owned());

                for parameter in parameters {
                    self.scope()?.define(Symbol::Var(parameter));
                }

                self.visit_block(block)?;
                self.leave_scope();

                Ok(())
            }
        };
    }

    pub fn visit_function_declaration(&mut self, node: &FunctionDeclaration) -> Result<(), String> {
        return match node {
            &FunctionDeclaration::Function(ref name, ref parameter_list, ref block, ref return_type) => {
                let parameters = self.visit_formal_parameter_list(parameter_list)?;

                self.scope()?.define(Symbol::Callable(CallableSymbol::Function(name.to_owned(), parameters.to_vec(), return_type.clone())));
                self.enter_scope(name.to_owned());

                for parameter in parameters {
                    self.scope()?.define(Symbol::Var(parameter));
                }

                match (self.visit_block(block)?, return_type) {
                    (TypeSpec::STRING, &TypeSpec::STRING)   => Ok(()),
                    (TypeSpec::INTEGER, &TypeSpec::INTEGER) => Ok(()),
                    (TypeSpec::REAL, &TypeSpec::REAL)       => Ok(()),
                    (TypeSpec::BOOLEAN, &TypeSpec::BOOLEAN) => Ok(()),
                    (actual, expected)                      => Err(String::from(
                        format!("Semantic Analyzer Error: Mismatching return types in function '{:?}'. Declared return type of {:?}, actual return type of {:?}", name, expected, actual)
                    ))
                }?;

                self.leave_scope();

                Ok(())
            }
        };
    }

    pub fn visit_formal_parameter_list(&mut self, node: &FormalParameterList) -> Result<Vec<VarSymbol>, String> {
        return match node {
            &FormalParameterList::FormalParameters(ref formal_parameters) => {
                let mut vars: Vec<VarSymbol> = vec![];
                for parameters in formal_parameters {
                    let mut other_vars = self.visit_formal_parameters(parameters)?;
                   vars.append(&mut other_vars);
                }
                Ok(vars.to_vec())
            }
        };
    }

    pub fn visit_formal_parameters(&mut self, node: &FormalParameters) -> Result<Vec<VarSymbol>, String> {
        return match node {
            &FormalParameters::Parameters(ref names, ref type_spec) => match type_spec {
                &TypeSpec::INTEGER => {
                    let mut vars: Vec<VarSymbol> = vec![];
                    for name in names {
                        vars.push(VarSymbol::INTEGER(name.to_owned()));
                    }
                    Ok(vars.to_vec())
                },
                &TypeSpec::REAL    => {
                    let mut vars: Vec<VarSymbol> = vec![];
                    for name in names {
                        vars.push(VarSymbol::REAL(name.to_owned()));
                    }
                    Ok(vars.to_vec())
                },
                &TypeSpec::STRING  => {
                    let mut vars: Vec<VarSymbol> = vec![];
                    for name in names {
                        vars.push(VarSymbol::STRING(name.to_owned()));
                    }
                    Ok(vars.to_vec())
                },
                &TypeSpec::BOOLEAN  => {
                    let mut vars: Vec<VarSymbol> = vec![];
                    for name in names {
                        vars.push(VarSymbol::BOOLEAN(name.to_owned()));
                    }
                    Ok(vars.to_vec())
                },
                &TypeSpec::UNIT     => Err(String::from("Internal Semantic Analyzer Error: Formal Parameter of type Unit"))
            }
        };
    }

    pub fn visit_variable_declaration(&mut self, node: &VariableDeclaration) -> Result<(), String> {
        match node {
            &VariableDeclaration::Variables(ref names, TypeSpec::REAL) => {
                for name in names {
                    match self.scope()?.local_lookup(name) {
                        None    => Ok(self.scope()?.define(Symbol::Var(VarSymbol::REAL(name.to_owned())))),
                        Some(_) => Err(String::from(format!("Semantic Analyzer Error: Variable declared more than once: {}", name)))
                    }?;
                }

                Ok(())
            },
            &VariableDeclaration::Variables(ref names, TypeSpec::INTEGER) => {
                for name in names {
                    match self.scope()?.local_lookup(name) {
                        None    => Ok(self.scope()?.define(Symbol::Var(VarSymbol::INTEGER(name.to_owned())))),
                        Some(_) => Err(String::from(format!("Semantic Analyzer Error: Variable declared more than once: {}", name)))
                    }?;
                }

                Ok(())
            },
            &VariableDeclaration::Variables(ref names, TypeSpec::STRING) => {
                for name in names {
                    match self.scope()?.local_lookup(name) {
                        None    => Ok(self.scope()?.define(Symbol::Var(VarSymbol::STRING(name.to_owned())))),
                        Some(_) => Err(String::from(format!("Semantic Analyzer Error: Variable declared more than once: {}", name)))
                    }?;
                }

                Ok(())
            },
            &VariableDeclaration::Variables(ref names, TypeSpec::BOOLEAN) => {
                for name in names {
                    match self.scope()?.local_lookup(name) {
                        None    => Ok(self.scope()?.define(Symbol::Var(VarSymbol::BOOLEAN(name.to_owned())))),
                        Some(_) => Err(String::from(format!("Variable declared more than once: {}", name)))
                    }?;
                }

                Ok(())
            },
            &VariableDeclaration::Variables(_, TypeSpec::UNIT)            => Err(String::from("Internal Semantic Analyzer Error: Variable Declaration of type Unit"))
        }?;

        return Ok(());
    }

    pub fn visit_compound(&mut self, node: &Compound) -> Result<TypeSpec, String> {
        return match node {
            &Compound::Statements(ref statements) => {
                let mut last_type = TypeSpec::UNIT;
                for statement in statements {
                    last_type = self.visit_statement(statement)?;
                }

                Ok(last_type)
            }
        };
    }

    pub fn visit_statement(&mut self, node: &Statement) -> Result<TypeSpec, String> {
        return match node {
            &Statement::Compound(ref compound)            => self.visit_compound(compound),
            &Statement::Assignment(ref assignment)        => self.visit_assignment(assignment),
            &Statement::IfStatement(ref if_statement)     => self.visit_if_statement(if_statement),
            &Statement::FunctionCall(ref function_call)   => self.visit_function_call(function_call),
        };
    }

    pub fn visit_if_statement(&mut self, node: &IfStatement) -> Result<TypeSpec, String> {
        return match node {
            &IfStatement::If(ref expr, ref compound_statement) => {
                match self.visit_expr(expr)? {
                    TypeSpec::BOOLEAN => Ok(()),
                    _                 => Err(String::from("Semantic Analyzer Error: If statement must use a boolean expression"))
                }?;

                self.visit_compound(compound_statement)?;
                Ok(TypeSpec::UNIT)
            },
            &IfStatement::IfElse(ref expr, ref if_compound_statement, ref else_compound_statement) => {
                match self.visit_expr(expr)? {
                    TypeSpec::BOOLEAN => Ok(()),
                    _                 => Err(String::from("Semantic Analyzer Error: If statement must use a boolean expression"))
                }?;

                self.visit_compound(if_compound_statement)?;
                self.visit_compound(else_compound_statement)?;
                Ok(TypeSpec::UNIT)
            },
            &IfStatement::IfElseIf(ref expr, ref if_compound_statement, ref else_if_statement) => {
                match self.visit_expr(expr)? {
                    TypeSpec::BOOLEAN => Ok(()),
                    _                 => Err(String::from("Semantic Analyzer Error: If statement must use a boolean expression"))
                }?;

                self.visit_compound(if_compound_statement)?;
                self.visit_if_statement(else_if_statement)?;
                Ok(TypeSpec::UNIT)
            }
        }
    }

    pub fn visit_assignment(&mut self, node: &Assignment) -> Result<TypeSpec, String> {
        return match node {
            &Assignment::Assign(Variable::Var(ref name), ref expression) => {
                let symbol = match self.scope()?.lookup(name) {
                    Some(symbol)  => Ok(symbol.clone()),
                    None          => Err(String::from(format!("Semantic Analyzer Error: Variable {} was never defined", name)))
                }?;

                match (symbol, self.visit_expr(expression)?) {
                    (Symbol::Var(VarSymbol::INTEGER(_)), TypeSpec::INTEGER) => Ok(TypeSpec::INTEGER),
                    (Symbol::Var(VarSymbol::REAL(_)), TypeSpec::REAL)       => Ok(TypeSpec::REAL),
                    (Symbol::Var(VarSymbol::STRING(_)), TypeSpec::STRING)   => Ok(TypeSpec::STRING),
                    (Symbol::Var(VarSymbol::BOOLEAN(_)), TypeSpec::BOOLEAN) => Ok(TypeSpec::BOOLEAN),
                    (Symbol::Var(var_symbol), TypeSpec::UNIT)               => Err(String::from(format!("Semantic Analyzer Error: Attempted to assign {:?} to a statement with no return type", var_symbol))),
                    (Symbol::Var(var_symbol), type_spec)                    => Err(String::from(format!("Semantic Analyzer Error: Mismatched Types, Attempted to assign {:?} to {:?}", type_spec, var_symbol))),
                    (Symbol::Callable(callable), _)                         => Err(String::from(format!("Semantic Analyzer Error: Attempted to assign expression to callable {:?}", callable)))
                }
            }
        };
    }

    pub fn visit_function_call(&mut self, node: &FunctionCall) -> Result<TypeSpec, String> {
        return match node {
            &FunctionCall::Call(Variable::Var(ref name), ref parameters) => {
                let callable =  match self.scope()?.lookup(name) {
                    Some(&Symbol::Callable(ref callable))   => Ok(callable.clone()),
                    _                                       => Err(String::from(format!("Semantic Analyzer Error: Unknown callable '{:?}'", name)))
                }?;
                let declared_params = match callable {
                    CallableSymbol::Procedure(_, ref declared_params)  => declared_params.to_vec(),
                    CallableSymbol::Function(_, ref declared_params, _) => declared_params.to_vec()
                };
                let given_params = self.visit_call_parameters(parameters)?;

                if declared_params.len() == given_params.len() {
                    for (declared, given) in declared_params.iter().zip(given_params.iter()) {
                        match (declared, given) {
                            (&VarSymbol::INTEGER(_), &TypeSpec::INTEGER) => Ok(()),
                            (&VarSymbol::REAL(_), &TypeSpec::REAL)       => Ok(()),
                            (&VarSymbol::STRING(_), &TypeSpec::STRING)   => Ok(()),
                            (&VarSymbol::BOOLEAN(_), &TypeSpec::BOOLEAN) => Ok(()),
                            (expected, actual)                           => Err(String::from(format!("Semantic Analyzer Error: Callable expected parameter of type {:?}, but {:?} was given", expected, actual)))
                        }?;
                    }
                    match callable {
                        CallableSymbol::Procedure(_, _)           => Ok(TypeSpec::UNIT),
                        CallableSymbol::Function(_, _, type_spec) => Ok(type_spec)
                    }
                } else {
                    Err(String::from(format!("Semantic Analyzer Error: Callable {:?} expected {:?} arguments, but {:?} were given", callable, declared_params.len(), given_params.len())))
                }
            }
        }
    }

    pub fn visit_call_parameters(&mut self, node: &CallParameters) -> Result<Vec<TypeSpec>, String> {
        return match node {
            &CallParameters::Parameters(ref expressions) => {
                let mut parameters: Vec<TypeSpec> = vec![];

                for expr in expressions.iter() {
                    parameters.push(self.visit_expr(expr)?);
                }

                Ok(parameters)
            }
        }
    }

    pub fn visit_expr(&mut self, node: &Expr) -> Result<TypeSpec, String> {
        return match node {
            &Expr::UnaryOp(ref unaryop_expr)       => self.visit_unaryop(unaryop_expr),
            &Expr::BinOp(ref binop_expr)           => self.visit_binop(binop_expr),
            &Expr::Group(ref group_expr)           => self.visit_group(group_expr),
            &Expr::Literal(ref literal)            => self.visit_literal(literal),
            &Expr::Variable(ref variable)          => self.visit_variable(variable),
            &Expr::FunctionCall(ref function_call) => self.visit_function_call(function_call)
        };
    }

    pub fn visit_unaryop(&mut self, expr: &UnaryOpExpr) -> Result<TypeSpec, String> {
        return match expr {
            &UnaryOpExpr::UnaryOp(ref operator, ref unary_expr) =>
                match (operator, self.visit_expr(unary_expr)?) {
                    (&UnaryOperator::Plus, TypeSpec::INTEGER)  => Ok(TypeSpec::INTEGER),
                    (&UnaryOperator::Minus, TypeSpec::INTEGER) => Ok(TypeSpec::INTEGER),
                    (&UnaryOperator::Plus, TypeSpec::REAL)     => Ok(TypeSpec::REAL),
                    (&UnaryOperator::Minus, TypeSpec::REAL)    => Ok(TypeSpec::REAL),
                    (&UnaryOperator::Not, TypeSpec::BOOLEAN)   => Ok(TypeSpec::BOOLEAN),
                    (operator, type_spec)                      => Err(String::from(
                        format!("Semantic Analyzer Error: Attempted to use unary operator {:?} with incompatible type {:?}", operator, type_spec)
                    ))
            }
        };
    }

    pub fn visit_binop(&mut self, expr: &BinaryOpExpr) -> Result<TypeSpec, String> {
        return match expr {
            &BinaryOpExpr::BinaryOp(ref left, ref operator, ref right) =>
                match (self.visit_expr(left)?, operator, self.visit_expr(right)?) {
                    (TypeSpec::INTEGER, &BinaryOperator::Plus, TypeSpec::INTEGER)               => Ok(TypeSpec::INTEGER),
                    (TypeSpec::INTEGER, &BinaryOperator::Minus, TypeSpec::INTEGER)              => Ok(TypeSpec::INTEGER),
                    (TypeSpec::INTEGER, &BinaryOperator::Multiply, TypeSpec::INTEGER)           => Ok(TypeSpec::INTEGER),
                    (TypeSpec::INTEGER, &BinaryOperator::IntegerDivide, TypeSpec::INTEGER)      => Ok(TypeSpec::INTEGER),
                    (TypeSpec::INTEGER, &BinaryOperator::LessThan, TypeSpec::INTEGER)           => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::INTEGER, &BinaryOperator::LessThanOrEqual, TypeSpec::INTEGER)    => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::INTEGER, &BinaryOperator::GreaterThan, TypeSpec::INTEGER)        => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::INTEGER, &BinaryOperator::GreaterThanOrEqual, TypeSpec::INTEGER) => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::INTEGER, &BinaryOperator::Equal, TypeSpec::INTEGER)              => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::INTEGER, &BinaryOperator::NotEqual, TypeSpec::INTEGER)           => Ok(TypeSpec::BOOLEAN),

                    (TypeSpec::REAL, &BinaryOperator::Plus, TypeSpec::REAL)                     => Ok(TypeSpec::REAL),
                    (TypeSpec::REAL, &BinaryOperator::Minus, TypeSpec::REAL)                    => Ok(TypeSpec::REAL),
                    (TypeSpec::REAL, &BinaryOperator::Multiply, TypeSpec::REAL)                 => Ok(TypeSpec::REAL),
                    (TypeSpec::REAL, &BinaryOperator::FloatDivide, TypeSpec::REAL)              => Ok(TypeSpec::REAL),
                    (TypeSpec::REAL, &BinaryOperator::LessThan, TypeSpec::REAL)                 => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::REAL, &BinaryOperator::LessThanOrEqual, TypeSpec::REAL)          => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::REAL, &BinaryOperator::GreaterThan, TypeSpec::REAL)              => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::REAL, &BinaryOperator::GreaterThanOrEqual, TypeSpec::REAL)       => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::REAL, &BinaryOperator::Equal, TypeSpec::REAL)                    => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::REAL, &BinaryOperator::NotEqual, TypeSpec::REAL)                 => Ok(TypeSpec::BOOLEAN),

                    (TypeSpec::STRING, &BinaryOperator::Plus, TypeSpec::STRING)                 => Ok(TypeSpec::STRING),
                    (TypeSpec::BOOLEAN, &BinaryOperator::And, TypeSpec::BOOLEAN)                => Ok(TypeSpec::BOOLEAN),
                    (TypeSpec::BOOLEAN, &BinaryOperator::Or, TypeSpec::BOOLEAN)                 => Ok(TypeSpec::BOOLEAN),

                    (TypeSpec::INTEGER, operator, TypeSpec::INTEGER)                            => Err(String::from(format!("Semantic Analyzer Error: Binary operator {:?} is incompatible with {:?} types", operator, TypeSpec::INTEGER))),
                    (TypeSpec::REAL, operator, TypeSpec::REAL)                                  => Err(String::from(format!("Semantic Analyzer Error: Binary operator {:?} is incompatible with {:?} types", operator, TypeSpec::REAL))),
                    (TypeSpec::STRING, operator, TypeSpec::STRING)                              => Err(String::from(format!("Semantic Analyzer Error: Binary operator {:?} is incompatible with {:?} types", operator, TypeSpec::STRING))),
                    (TypeSpec::BOOLEAN, operator, TypeSpec::BOOLEAN)                            => Err(String::from(format!("Semantic Analyzer Error: Binary operator {:?} is incompatible with {:?} types", operator, TypeSpec::BOOLEAN))),
                    (type_spec_left, operator, type_spec_right)                                 => Err(String::from(format!("Semantic Analyzer Error: Attempted to use binary operator {:?} with mismatching types {:?} and {:?}", operator, type_spec_left, type_spec_right)))
                }
        };
    }

    fn visit_group(&mut self, expr: &GroupedExpr) -> Result<TypeSpec, String> {
        return match expr {
            &GroupedExpr::Group(ref grouped_expr) => self.visit_expr(grouped_expr)
        };
    }

    pub fn visit_literal(&mut self, expr: &Literal) -> Result<TypeSpec, String> {
        return match expr {
            &Literal::Int(_)     => Ok(TypeSpec::INTEGER),
            &Literal::Float(_)   => Ok(TypeSpec::REAL),
            &Literal::String(_)  => Ok(TypeSpec::STRING),
            &Literal::Boolean(_) => Ok(TypeSpec::BOOLEAN)
        }
    }

    pub fn visit_variable(&mut self, node: &Variable) -> Result<TypeSpec, String> {
        return match node {
            &Variable::Var(ref name) => {
                match self.scope()?.lookup(name) {
                    Some(symbol) => match symbol {
                        &Symbol::Var(VarSymbol::INTEGER(_)) => Ok(TypeSpec::INTEGER),
                        &Symbol::Var(VarSymbol::REAL(_))    => Ok(TypeSpec::REAL),
                        &Symbol::Var(VarSymbol::STRING(_))  => Ok(TypeSpec::STRING),
                        &Symbol::Var(VarSymbol::BOOLEAN(_)) => Ok(TypeSpec::BOOLEAN),
                        &Symbol::Callable(ref callable)     => Err(String::from(format!("Semantic Analyzer Error: Attempted to use callable {:?} as a variable", callable)))
                    },
                    None         => Err(String::from(format!("Semantic Analyzer Error: Unknown variable with name {}", name)))
                }
            }
        };
    }

    pub fn enter_scope(&mut self, name: String) {
        let current_scope = self.scope.take();
        match current_scope {
            Some(scope) => self.scope = Some(SymbolTable::with_enclosing_scope(name, scope)),
            None        => self.scope = Some(SymbolTable::new(name))
        };
    }

    pub fn leave_scope(&mut self) {
        let current_scope = self.scope.take();

        match current_scope {
            Some(scope) => self.scope = scope.enclosing_scope(),
            None        => self.scope = None
        };
    }

    pub fn scope(&mut self) -> Result<&mut SymbolTable, String> {
        return match self.scope {
            Some(ref mut scope) => Ok(scope),
            None                => Err(String::from("Internal Semantic Analyzer Error: Unknown Scope"))
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn visit_program() {
        let mut analyzer = SemanticAnalyzer::new();
        let program = Program::Program(Variable::Var(String::from("test")), Block::Block(vec![], Compound::Statements(vec![])));

        assert_eq!(analyzer.visit_program(&program), Ok(()));
    }

    #[test]
    fn visit_block() {
        let mut analyzer = SemanticAnalyzer::new();
        let block = Block::Block(vec![], Compound::Statements(vec![]));

        assert_eq!(analyzer.visit_block(&block), Ok(TypeSpec::UNIT));
    }

    #[test]
    fn visit_declarations() {
        let mut analyzer = SemanticAnalyzer::new();
        let declarations = vec![
            Declarations::VariableDeclarations(vec![]),
            Declarations::FunctionDeclarations(vec![]),
            Declarations::ProcedureDeclarations(vec![]),
            Declarations::Empty
        ];

        assert_eq!(analyzer.visit_declarations(&declarations), Ok(()));
    }

    #[test]
    fn visit_procedure_declaration() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.enter_scope(String::from("test_scope"));

        let procedure_declaration = ProcedureDeclaration::Procedure(
            String::from("test"),
            FormalParameterList::FormalParameters(vec![]),
            Block::Block(vec![], Compound::Statements(vec![]))
        );

        assert_eq!(analyzer.visit_procedure_declaration(&procedure_declaration), Ok(()));
        assert_eq!(
            analyzer.scope().unwrap().lookup(&String::from("test")),
            Some(&Symbol::Callable(CallableSymbol::Procedure(String::from("test"), vec![])))
        );
    }

    #[test]
    fn visit_function_declaration() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.enter_scope(String::from("test_scope"));

        let function_declaration = FunctionDeclaration::Function(
            String::from("test"),
            FormalParameterList::FormalParameters(vec![]),
            Block::Block(vec![
                // declare variable 'foobar' of type integer
                Declarations::VariableDeclarations(vec![
                    VariableDeclaration::Variables(
                        vec![String::from("foobar")],
                        TypeSpec::INTEGER
                    )
                ])
                ],
                Compound::Statements(vec![
                    Statement::Assignment(
                        // assign variable 'foobar' to integer 5 (makes return value of function an integer)
                        Assignment::Assign(
                            Variable::Var(String::from("foobar")),
                            Expr::Literal(Literal::Int(5))
                        )
                    )
                ])
            ),
            TypeSpec::INTEGER
        );

        assert_eq!(analyzer.visit_function_declaration(&function_declaration), Ok(()));
        assert_eq!(
            analyzer.scope().unwrap().lookup(&String::from("test")),
            Some(&Symbol::Callable(CallableSymbol::Function(String::from("test"), vec![], TypeSpec::INTEGER)))
        );
    }

    #[test]
    fn visit_formal_parameter_list() {
        let mut analyzer = SemanticAnalyzer::new();
        let formal_parameter_list = FormalParameterList::FormalParameters(vec![
            FormalParameters::Parameters(vec![String::from("a"), String::from("b")], TypeSpec::INTEGER),
            FormalParameters::Parameters(vec![String::from("c"), String::from("d")], TypeSpec::REAL),
            FormalParameters::Parameters(vec![String::from("e"), String::from("f")], TypeSpec::STRING),
            FormalParameters::Parameters(vec![String::from("g"), String::from("h")], TypeSpec::BOOLEAN),
        ]);

        assert_eq!(
            analyzer.visit_formal_parameter_list(&formal_parameter_list),
            Ok(vec![
                VarSymbol::INTEGER(String::from("a")), VarSymbol::INTEGER(String::from("b")),
                VarSymbol::REAL(String::from("c")), VarSymbol::REAL(String::from("d")),
                VarSymbol::STRING(String::from("e")), VarSymbol::STRING(String::from("f")),
                VarSymbol::BOOLEAN(String::from("g")), VarSymbol::BOOLEAN(String::from("h")),
            ])
        );
    }

    #[test]
    fn visit_formal_parameters() {
        let mut analyzer = SemanticAnalyzer::new();
        let formal_parameters = FormalParameters::Parameters(vec![String::from("a"), String::from("b")], TypeSpec::INTEGER);

        assert_eq!(
            analyzer.visit_formal_parameters(&formal_parameters),
            Ok(vec![VarSymbol::INTEGER(String::from("a")), VarSymbol::INTEGER(String::from("b"))])
        );
    }

    #[test]
    fn visit_variable_declaration() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.enter_scope(String::from("test_scope"));
        let variable_declaration = VariableDeclaration::Variables(vec![String::from("a"), String::from("b")], TypeSpec::INTEGER);

        assert_eq!(analyzer.visit_variable_declaration(&variable_declaration), Ok(()));
        assert_eq!(analyzer.scope().unwrap().lookup(&String::from("a")), Some(&Symbol::Var(VarSymbol::INTEGER(String::from("a")))));
        assert_eq!(analyzer.scope().unwrap().lookup(&String::from("b")), Some(&Symbol::Var(VarSymbol::INTEGER(String::from("b")))));
    }

    #[test]
    fn visit_compound() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.enter_scope(String::from("test_scope"));
        analyzer.scope().unwrap().define(Symbol::Var(VarSymbol::INTEGER(String::from("foo"))));
        analyzer.scope().unwrap().define(Symbol::Var(VarSymbol::BOOLEAN(String::from("bar"))));

        let compound = Compound::Statements(vec![
            Statement::Assignment(Assignment::Assign(Variable::Var(String::from("foo")), Expr::Literal(Literal::Int(5)))),
            Statement::Assignment(Assignment::Assign(Variable::Var(String::from("bar")), Expr::Literal(Literal::Boolean(true)))),
        ]);

        assert_eq!(analyzer.visit_compound(&compound), Ok(TypeSpec::BOOLEAN));
    }

    #[test]
    fn visit_if_statement() {
        let mut analyzer = SemanticAnalyzer::new();
        let if_statement = IfStatement::IfElseIf(             // if false then
            Expr::Literal(Literal::Boolean(false)),        //     begin
            Compound::Statements(vec![]),                  //     end
            Box::new(                                      // else if true then
                IfStatement::IfElse(                       //     begin
                    Expr::Literal(Literal::Boolean(true)), //     end
                    Compound::Statements(vec![]),          // else
                    Compound::Statements(vec![])           //     begin
                )                                             //     end
            )
        );

        assert_eq!(analyzer.visit_if_statement(&if_statement), Ok(TypeSpec::UNIT));
    }

    #[test]
    fn visit_assignment() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.enter_scope(String::from("test_scope"));
        analyzer.scope().unwrap().define(Symbol::Var(VarSymbol::INTEGER(String::from("test"))));
        let assignment = Assignment::Assign(
            Variable::Var(String::from("test")),
            Expr::Literal(Literal::Int(5))
        );

        assert_eq!(analyzer.visit_assignment(&assignment), Ok(TypeSpec::INTEGER));
    }

    #[test]
    fn visit_function_call() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.enter_scope(String::from("test_scope"));
        analyzer.scope().unwrap().define(
            Symbol::Callable(
                CallableSymbol::Function(
                    String::from("test"),
                    vec![
                        VarSymbol::INTEGER(String::from("a")),
                        VarSymbol::INTEGER(String::from("b")),
                        VarSymbol::BOOLEAN(String::from("c"))
                    ],
                    TypeSpec::REAL
                )
            )
        );
        let function_call = FunctionCall::Call(
            Variable::Var(String::from("test")),
            CallParameters::Parameters(
                vec![
                    Expr::Literal(Literal::Int(5)),
                    Expr::Literal(Literal::Int(10)),
                    Expr::Literal(Literal::Boolean(true))
                ]
            )
        );

        assert_eq!(analyzer.visit_function_call(&function_call), Ok(TypeSpec::REAL));
    }

    #[test]
    fn visit_call_parameters() {
        let mut analyzer = SemanticAnalyzer::new();
        let call_parameters = CallParameters::Parameters(
            vec![
                Expr::Literal(Literal::Int(5)),
                Expr::Literal(Literal::Int(10)),
                Expr::Literal(Literal::Boolean(true))
            ]
        );

        assert_eq!(
            analyzer.visit_call_parameters(&call_parameters),
            Ok(vec![TypeSpec::INTEGER, TypeSpec::INTEGER, TypeSpec::BOOLEAN])
        );
    }

    #[test]
    fn visit_unaryop_expr() {
        let mut analyzer = SemanticAnalyzer::new();
        let unaryop_expr = UnaryOpExpr::UnaryOp(
                UnaryOperator::Minus,
                Expr::Literal(Literal::Float(5.0))
        );

        assert_eq!(analyzer.visit_unaryop(&unaryop_expr), Ok(TypeSpec::REAL));
    }

    #[test]
    fn visit_binop_expr() {
        let mut analyzer = SemanticAnalyzer::new();
        let binop_expr = BinaryOpExpr::BinaryOp(
            Expr::Literal(Literal::Int(5)),
            BinaryOperator::Multiply,
            Expr::Literal(Literal::Int(10))
        );

        assert_eq!(analyzer.visit_binop(&binop_expr), Ok(TypeSpec::INTEGER));
    }

    #[test]
    fn visit_group_expr() {
        let mut analyzer = SemanticAnalyzer::new();
        let group_expr = GroupedExpr::Group(Expr::Literal(Literal::Boolean(true)));

        assert_eq!(analyzer.visit_group(&group_expr), Ok(TypeSpec::BOOLEAN));
    }

    #[test]
    fn visit_literal() {
        let mut analyzer = SemanticAnalyzer::new();
        let literal = Literal::String(String::from("test"));

        assert_eq!(analyzer.visit_literal(&literal), Ok(TypeSpec::STRING));
    }

    #[test]
    fn visit_variable() {
        let mut analyzer = SemanticAnalyzer::new();
        analyzer.enter_scope(String::from("test_scope"));
        analyzer.scope().unwrap().define(Symbol::Var(VarSymbol::REAL(String::from("test"))));
        let variable = Variable::Var(String::from("test"));

        assert_eq!(analyzer.visit_variable(&variable), Ok(TypeSpec::REAL));
    }
}