use parser::ast::Program;
use parser::ast::Block;
use parser::ast::Declarations;
use parser::ast::ProcedureDeclaration;
use parser::ast::FormalParameterList;
use parser::ast::FormalParameters;
use parser::ast::VariableDeclaration;
use parser::ast::Compound;
use parser::ast::StatementList;
use parser::ast::Statement;
use parser::ast::TypeSpec;
use parser::ast::ProcedureCall;
use parser::ast::ProcedureParameters;
use parser::ast::Expr;
use parser::ast::Assignment;
use parser::ast::Variable;
use parser::ast::Operator;

use super::symbol_table::SymbolTable;
use super::symbol::Symbol;
use super::symbol::VarSymbol;
use super::symbol::ProcedureSymbol;

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

    fn visit_program(&mut self, node: &Program) -> Result<(), String> {
        return match node {
            &Program::Program(ref var, ref block) => {
                match var {
                    &Variable::Var(ref name) => {
                        self.enter_scope(name.to_owned());
                    }
                };

                self.visit_block(block)?;
                self.leave_scope();

                Ok(())
            }
        };
    }

    fn visit_block(&mut self, node: &Block) -> Result<(), String> {
        return match node {
            &Block::Block(ref declarations, ref compound) => {
                self.visit_declarations(declarations)?;
                self.visit_compound(compound)?;

                Ok(())
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

                self.scope()?.define(Symbol::Procedure(ProcedureSymbol::Procedure(name.to_owned(), parameters.to_vec())));
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
                &TypeSpec::REAL => {
                    let mut vars: Vec<VarSymbol> = vec![];
                    for name in names {
                        vars.push(VarSymbol::REAL(name.to_owned()));
                    }
                    Ok(vars.to_vec())
                }
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
            },
            &VariableDeclaration::Variables(ref names, TypeSpec::INTEGER) => {
                for name in names {
                    match self.scope()?.local_lookup(name) {
                        None    => Ok(self.scope()?.define(Symbol::Var(VarSymbol::INTEGER(name.to_owned())))),
                        Some(_) => Err(String::from(format!("Variable declared more than once: {}", name)))
                    }?;
                }
            }
        };

        return Ok(());
    }

    fn visit_compound(&mut self, node: &Compound) -> Result<(), String> {
        return match node {
            &Compound::StatementList(ref statement_list) => self.visit_statement_list(statement_list)
        };
    }

    fn visit_statement_list(&mut self, node: &StatementList) -> Result<(), String> {
        match node {
            &StatementList::Statements(ref statements) => {
                for statement in statements {
                    self.visit_statement(statement)?;
                }
            }
        };

        return Ok(());
    }

    fn visit_statement(&mut self, node: &Statement) -> Result<(), String> {
        return match node {
            &Statement::Compound(ref compound)            => self.visit_compound(compound),
            &Statement::Assignment(ref assignment)        => self.visit_assignment(assignment),
            &Statement::ProcedureCall(ref procedure_call) => self.visit_procedure_call(procedure_call),
            &Statement::Empty                             => Ok(())
        };
    }

    fn visit_assignment(&mut self, node: &Assignment) -> Result<(), String> {
        return match node {
            &Assignment::Assign(Variable::Var(ref name), ref expression) => {
                let symbol = match self.scope()?.lookup(name) {
                    Some(symbol)  => Ok(symbol.clone()),
                    None          => Err(String::from(format!("Variable {} was never defined", name)))
                }?;

                match symbol {
                    Symbol::Var(VarSymbol::INTEGER(_)) =>
                        match self.visit_expr(expression)? {
                            TypeSpec::INTEGER  => Ok(()),
                            _                  => Err(String::from("Can't assign mismatched types"))
                        },
                    Symbol::Var(VarSymbol::REAL(_))    =>
                        match self.visit_expr(expression)? {
                            TypeSpec::REAL => Ok(()),
                            _              => Err(String::from("Can't assign mismatched types"))
                        },
                    _                                   => Err(String::from("Can't assign procedure"))

                }
            }
        };
    }

    fn visit_procedure_call(&mut self, node: &ProcedureCall) -> Result<(), String> {
        return match node {
            &ProcedureCall::Call(Variable::Var(ref name), ref parameters) => {
                let procedure =  match self.scope()?.lookup(name) {
                    Some(&Symbol::Procedure(ref procedure)) => Ok(procedure.clone()),
                    _                                       => Err(String::from("Unknown procedure"))
                }?;
                let declared_params = match procedure {
                    ProcedureSymbol::Procedure(_, declared_params) => declared_params
                };
                let given_params = self.visit_procedure_parameters(parameters)?;

                if declared_params.len() == given_params.len() {
                    for (declared, given) in declared_params.iter().zip(given_params.iter()) {
                        match (declared, given) {
                            (&VarSymbol::INTEGER(_), &VarSymbol::INTEGER(_)) => Ok(()),
                            (&VarSymbol::REAL(_), &VarSymbol::REAL(_))       => Ok(()),
                            (expected, actual)                               => Err(String::from(format!("Procedure expected {:?}, but {:?} was given", expected, actual)))
                        }?;
                    }
                    Ok(())
                } else {
                    Err(String::from("Wrong number of arguments"))
                }
            }
        }
    }

    fn visit_procedure_parameters(&mut self, node: &ProcedureParameters) -> Result<Vec<VarSymbol>, String> {
        return match node {
            &ProcedureParameters::Parameters(ref variables) => {
                let mut parameters: Vec<VarSymbol> = vec![];
                let mut variable_iter = variables.iter();
                while let Some(&Variable::Var(ref name)) = variable_iter.next() {
                    match self.scope()?.lookup(name) {
                        Some(&Symbol::Var(ref var_symbol)) => {
                            parameters.push(var_symbol.clone());
                            Ok(())
                        },
                        _                                  => Err(String::from(format!("Unknown variable: {}", name)))
                    }?;
                }

                Ok(parameters)
            }
        }
    }

    fn visit_expr(&mut self, node: &Expr) -> Result<TypeSpec, String> {
        return match node {
            &Expr::BinOp(_, _, _)         => self.visit_binop(node),
            &Expr::UnaryOp(_, _)          => self.visit_unaryop(node),
            &Expr::Int(_)                 => self.visit_int(node),
            &Expr::Float(_)               => self.visit_float(node),
            &Expr::Variable(ref variable) => self.visit_variable(variable)
        };
    }

    fn visit_binop(&mut self, expr: &Expr) -> Result<TypeSpec, String> {
        return match expr {
            &Expr::BinOp(ref left, Operator::IntegerDivide, ref right) =>
                match (self.visit_expr(left)?, self.visit_expr(right)?) {
                    (TypeSpec::INTEGER, TypeSpec::INTEGER) => Ok(TypeSpec::INTEGER),
                    (TypeSpec::REAL, TypeSpec::REAL)       => Err(String::from("Cannot do integer division with float types")),
                    _                                      => Err(String::from("Mismatching types"))
                },
            &Expr::BinOp(ref left, Operator::FloatDivide, ref right) =>
                match (self.visit_expr(left)?, self.visit_expr(right)?) {
                    (TypeSpec::REAL, TypeSpec::REAL)       => Ok(TypeSpec::REAL),
                    (TypeSpec::INTEGER, TypeSpec::INTEGER) => Err(String::from("Cannot do float division with integer types")),
                    _                                      => Err(String::from("Mismatching types"))
                },
            &Expr::BinOp(ref left, _, ref right) =>
                match (self.visit_expr(left)?, self.visit_expr(right)?) {
                    (TypeSpec::INTEGER, TypeSpec::INTEGER) => Ok(TypeSpec::INTEGER),
                    (TypeSpec::REAL, TypeSpec::REAL)       => Ok(TypeSpec::REAL),
                    _                                      => Err(String::from("Mismatching types"))
                },
            _                                              => Err(String::from("Semantic Error"))
        };
    }

    fn visit_unaryop(&mut self, expr: &Expr) -> Result<TypeSpec, String> {
        return match expr {
            &Expr::UnaryOp(_, ref factor) => self.visit_expr(factor),
            _                             => Err(String::from("Semantic Error"))
        };
    }


    fn visit_int(&mut self, expr: &Expr) -> Result<TypeSpec, String> {
        return match expr {
            &Expr::Int(_) => Ok(TypeSpec::INTEGER),
            _             => Err(String::from("Semantic Error"))
        }
    }

    fn visit_float(&mut self, expr: &Expr) -> Result<TypeSpec, String> {
        return match expr {
            &Expr::Float(_) => Ok(TypeSpec::REAL),
            _               => Err(String::from("Semantic Error"))
        }
    }

    fn visit_variable(&mut self, node: &Variable) -> Result<TypeSpec, String> {
        return match node {
            &Variable::Var(ref name) => {
                match self.scope()?.lookup(name) {
                    Some(symbol) => match symbol {
                        &Symbol::Var(VarSymbol::INTEGER(_)) => Ok(TypeSpec::INTEGER),
                        &Symbol::Var(VarSymbol::REAL(_))    => Ok(TypeSpec::REAL),
                        _                                   => Err(String::from("Procedure cannot be a variable"))
                    },
                    None         => Err(String::from(format!("Unknown variable: {}", name)))
                }
            }
        }
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