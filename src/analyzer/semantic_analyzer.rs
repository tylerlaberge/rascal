use parser::ast::Program;
use parser::ast::Block;
use parser::ast::Declarations;
use parser::ast::ProcedureDeclaration;
use parser::ast::FormalParameterList;
use parser::ast::FormalParameters;
use parser::ast::VariableDeclaration;
use parser::ast::FunctionDeclaration;
use parser::ast::Compound;
use parser::ast::StatementList;
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
        println!("\n======================================== Analyzing ========================================\n");
        match self.visit_program(program) {
            Ok(()) => (),
            Err(e) => panic!("{}", e)
        }
    }

    fn init_built_ins(&mut self) -> Result<(), String> {
        self.scope()?.define(built_ins::writeln_procedure());

        return Ok(());
    }

    fn visit_program(&mut self, node: &Program) -> Result<(), String> {
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

    fn visit_block(&mut self, node: &Block) -> Result<TypeSpec, String> {
        return match node {
            &Block::Block(ref declarations, ref compound) => {
                self.visit_declarations(declarations)?;
                let return_type = self.visit_compound(compound)?;

                Ok(return_type)
            }
        }
    }

    fn visit_declarations(&mut self, node: &Vec<Declarations>) -> Result<(), String> {
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

    fn visit_procedure_declaration(&mut self, node: &ProcedureDeclaration) -> Result<(), String> {
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

    fn visit_function_declaration(&mut self, node: &FunctionDeclaration) -> Result<(), String> {
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
                    (actual, expected)                      => Err(String::from(format!("Mismatching return types. Declared return type of {:?}, actual return type of {:?}", expected, actual)))
                }?;

                self.leave_scope();

                Ok(())
            }
        };
    }

    fn visit_formal_parameter_list(&mut self, node: &FormalParameterList) -> Result<Vec<VarSymbol>, String> {
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

    fn visit_formal_parameters(&mut self, node: &FormalParameters) -> Result<Vec<VarSymbol>, String> {
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
                _                  => Err(String::from("Semantic Error"))
            }
        };
    }

    fn visit_variable_declaration(&mut self, node: &VariableDeclaration) -> Result<(), String> {
        match node {
            &VariableDeclaration::Variables(ref names, TypeSpec::REAL) => {
                for name in names {
                    match self.scope()?.local_lookup(name) {
                        None    => Ok(self.scope()?.define(Symbol::Var(VarSymbol::REAL(name.to_owned())))),
                        Some(_) => Err(String::from(format!("Variable declared more than once: {}", name)))
                    }?;
                }

                Ok(())
            },
            &VariableDeclaration::Variables(ref names, TypeSpec::INTEGER) => {
                for name in names {
                    match self.scope()?.local_lookup(name) {
                        None    => Ok(self.scope()?.define(Symbol::Var(VarSymbol::INTEGER(name.to_owned())))),
                        Some(_) => Err(String::from(format!("Variable declared more than once: {}", name)))
                    }?;
                }

                Ok(())
            },
            &VariableDeclaration::Variables(ref names, TypeSpec::STRING) => {
                for name in names {
                    match self.scope()?.local_lookup(name) {
                        None    => Ok(self.scope()?.define(Symbol::Var(VarSymbol::STRING(name.to_owned())))),
                        Some(_) => Err(String::from(format!("Variable declared more than once: {}", name)))
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
            _                                                            => Err(String::from("Semantic Error"))
        }?;

        return Ok(());
    }

    fn visit_compound(&mut self, node: &Compound) -> Result<TypeSpec, String> {
        return match node {
            &Compound::StatementList(ref statement_list) => self.visit_statement_list(statement_list)
        };
    }

    fn visit_statement_list(&mut self, node: &StatementList) -> Result<TypeSpec, String> {
        return match node {
            &StatementList::Statements(ref statements) => {
                let mut last_type = TypeSpec::UNIT;
                for statement in statements {
                    last_type = self.visit_statement(statement)?;
                }

                Ok(last_type)
            }
        };
    }

    fn visit_statement(&mut self, node: &Statement) -> Result<TypeSpec, String> {
        return match node {
            &Statement::Compound(ref compound)            => self.visit_compound(compound),
            &Statement::Assignment(ref assignment)        => self.visit_assignment(assignment),
            &Statement::IfStatement(ref if_statement)     => self.visit_if_statement(if_statement),
            &Statement::FunctionCall(ref function_call)   => self.visit_function_call(function_call),
            &Statement::Empty                             => Ok(TypeSpec::UNIT)
        };
    }

    fn visit_if_statement(&mut self, node: &IfStatement) -> Result<TypeSpec, String> {
        return match node {
            &IfStatement::If(ref expr, ref compound_statement) => {
                match self.visit_expr(expr)? {
                    TypeSpec::BOOLEAN => Ok(()),
                    _                 => Err(String::from("If statement must use a boolean expression"))
                }?;

                self.visit_compound(compound_statement)?;
                Ok(TypeSpec::UNIT)
            },
            &IfStatement::IfElse(ref expr, ref if_compound_statement, ref else_compound_statement) => {
                match self.visit_expr(expr)? {
                    TypeSpec::BOOLEAN => Ok(()),
                    _                 => Err(String::from("If statement must use a boolean expression"))
                }?;

                self.visit_compound(if_compound_statement)?;
                self.visit_compound(else_compound_statement)?;
                Ok(TypeSpec::UNIT)
            },
            &IfStatement::IfElseIf(ref expr, ref if_compound_statement, ref else_if_statement) => {
                match self.visit_expr(expr)? {
                    TypeSpec::BOOLEAN => Ok(()),
                    _                 => Err(String::from("If statement must use a boolean expression"))
                }?;

                self.visit_compound(if_compound_statement)?;
                self.visit_if_statement(else_if_statement)?;
                Ok(TypeSpec::UNIT)
            }
        }
    }

    fn visit_assignment(&mut self, node: &Assignment) -> Result<TypeSpec, String> {
        return match node {
            &Assignment::Assign(Variable::Var(ref name), ref expression) => {
                let symbol = match self.scope()?.lookup(name) {
                    Some(symbol)  => Ok(symbol.clone()),
                    None          => Err(String::from(format!("Variable {} was never defined", name)))
                }?;

                match (symbol, self.visit_expr(expression)?) {
                    (Symbol::Var(VarSymbol::INTEGER(_)), TypeSpec::INTEGER) => Ok(TypeSpec::INTEGER),
                    (Symbol::Var(VarSymbol::REAL(_)), TypeSpec::REAL)       => Ok(TypeSpec::REAL),
                    (Symbol::Var(VarSymbol::STRING(_)), TypeSpec::STRING)   => Ok(TypeSpec::STRING),
                    (Symbol::Var(VarSymbol::BOOLEAN(_)), TypeSpec::BOOLEAN) => Ok(TypeSpec::BOOLEAN),
                    (_, TypeSpec::UNIT)                                     => Err(String::from("Can't assign procedure call")),
                    (_, _)                                                  => Err(String::from("Can't assign mismatched types"))
                }
            }
        };
    }

    fn visit_function_call(&mut self, node: &FunctionCall) -> Result<TypeSpec, String> {
        return match node {
            &FunctionCall::Call(Variable::Var(ref name), ref parameters) => {
                let callable =  match self.scope()?.lookup(name) {
                    Some(&Symbol::Callable(ref callable))   => Ok(callable.clone()),
                    _                                       => Err(String::from("Unknown callable"))
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
                            (expected, actual)                           => Err(String::from(format!("Callable expected {:?}, but {:?} was given", expected, actual)))
                        }?;
                    }
                    match callable {
                        CallableSymbol::Procedure(_, _)           => Ok(TypeSpec::UNIT),
                        CallableSymbol::Function(_, _, type_spec) => Ok(type_spec)
                    }
                } else {
                    Err(String::from("Wrong number of arguments"))
                }
            }
        }
    }

    fn visit_call_parameters(&mut self, node: &CallParameters) -> Result<Vec<TypeSpec>, String> {
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

    fn visit_expr(&mut self, node: &Expr) -> Result<TypeSpec, String> {
        return match node {
            &Expr::UnaryOp(ref unaryop_expr)       => self.visit_unaryop(unaryop_expr),
            &Expr::BinOp(ref binop_expr)           => self.visit_binop(binop_expr),
            &Expr::Group(ref group_expr)           => self.visit_group(group_expr),
            &Expr::Literal(ref literal)            => self.visit_literal(literal),
            &Expr::Variable(ref variable)          => self.visit_variable(variable),
            &Expr::FunctionCall(ref function_call) => self.visit_function_call(function_call)
        };
    }

    fn visit_unaryop(&mut self, expr: &UnaryOpExpr) -> Result<TypeSpec, String> {
        return match expr {
            &UnaryOpExpr::UnaryOp(ref operator, ref unary_expr) =>
                match (operator, self.visit_expr(unary_expr)?) {
                    (&UnaryOperator::Plus, TypeSpec::INTEGER)  => Ok(TypeSpec::INTEGER),
                    (&UnaryOperator::Minus, TypeSpec::INTEGER) => Ok(TypeSpec::INTEGER),
                    (&UnaryOperator::Plus, TypeSpec::REAL)     => Ok(TypeSpec::REAL),
                    (&UnaryOperator::Minus, TypeSpec::REAL)    => Ok(TypeSpec::REAL),
                    (&UnaryOperator::Not, TypeSpec::BOOLEAN)   => Ok(TypeSpec::BOOLEAN),
                    _                                          => Err(String::from("Mismatching Types"))
            }
        };
    }

    fn visit_binop(&mut self, expr: &BinaryOpExpr) -> Result<TypeSpec, String> {
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
                    _                                                                           => Err(String::from("Mismatching types"))
                }
        };
    }

    fn visit_group(&mut self, expr: &GroupedExpr) -> Result<TypeSpec, String> {
        return match expr {
            &GroupedExpr::Group(ref grouped_expr) => self.visit_expr(grouped_expr)
        };
    }

    fn visit_literal(&mut self, expr: &Literal) -> Result<TypeSpec, String> {
        return match expr {
            &Literal::Int(_)     => Ok(TypeSpec::INTEGER),
            &Literal::Float(_)   => Ok(TypeSpec::REAL),
            &Literal::String(_)  => Ok(TypeSpec::STRING),
            &Literal::Boolean(_) => Ok(TypeSpec::BOOLEAN)
        }
    }

    fn visit_variable(&mut self, node: &Variable) -> Result<TypeSpec, String> {
        return match node {
            &Variable::Var(ref name) => {
                match self.scope()?.lookup(name) {
                    Some(symbol) => match symbol {
                        &Symbol::Var(VarSymbol::INTEGER(_)) => Ok(TypeSpec::INTEGER),
                        &Symbol::Var(VarSymbol::REAL(_))    => Ok(TypeSpec::REAL),
                        &Symbol::Var(VarSymbol::STRING(_))  => Ok(TypeSpec::STRING),
                        &Symbol::Var(VarSymbol::BOOLEAN(_)) => Ok(TypeSpec::BOOLEAN),
                        _                                   => Err(String::from("Procedure cannot be a variable"))
                    },
                    None         => Err(String::from(format!("Unknown variable: {}", name)))
                }
            }
        };
    }

    fn enter_scope(&mut self, name: String) {
        let current_scope = self.scope.take();
        match current_scope {
            Some(scope) => self.scope = Some(SymbolTable::with_enclosing_scope(name, scope)),
            None        => self.scope = Some(SymbolTable::new(name))
        };
    }

    fn leave_scope(&mut self) {
        let current_scope = self.scope.take();
        println!("{:?}", current_scope);

        match current_scope {
            Some(scope) => self.scope = scope.enclosing_scope(),
            None        => self.scope = None
        };
    }

    fn scope(&mut self) -> Result<&mut SymbolTable, String> {
        return match self.scope {
            Some(ref mut scope) => Ok(scope),
            None                => Err(String::from("Unknown Scope"))
        };
    }
}