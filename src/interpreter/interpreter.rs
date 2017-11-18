use parser::ast::Program;
use parser::ast::Block;
use parser::ast::Declarations;
use parser::ast::ProcedureDeclaration;
use parser::ast::FormalParameterList;
use parser::ast::FormalParameters;
use parser::ast::Compound;
use parser::ast::StatementList;
use parser::ast::Statement;
use parser::ast::ProcedureCall;
use parser::ast::ProcedureParameters;
use parser::ast::Expr;
use parser::ast::Assignment;
use parser::ast::Variable;
use parser::ast::Operator;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt;

use super::scope::Scope;
use super::object::Object;
use super::object::Primitive;

pub struct Interpreter {
    scope: Option<Scope>
}

impl Display for Interpreter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        return write!(f, "{:?}", self.scope);
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        return Interpreter {
            scope: None,
        };
    }

    pub fn interpret(&mut self, program: &Program) {
        println!("\n======================================== Interpreting ========================================\n");
        match self.visit_program(program) {
            Ok(()) => println!("{}", self),
            Err(e) => panic!("{}", e)
        };
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
        };
    }

    fn visit_declarations(&mut self, node: &Vec<Declarations>) -> Result<(), String> {
        for declarations in node {
            match declarations {
                &Declarations::ProcedureDeclarations(ref procedure_declarations) => {
                    for procedure_declaration in procedure_declarations {
                        self.visit_procedure_declaration(procedure_declaration)?;
                    }
                },
                _                                                                => ()
            };
        }
        return Ok(());
    }

    fn visit_procedure_declaration(&mut self, node: &ProcedureDeclaration) -> Result<(), String> {
        return match node {
            &ProcedureDeclaration::Procedure(ref name, ref parameter_list, ref block) => {
                let parameters = self.visit_formal_parameter_list(parameter_list)?;

                self.scope()?.set(name.to_owned(), Object::Procedure(name.to_owned(), parameters, block.clone()));

                Ok(())
            }
        };
    }

    fn visit_formal_parameter_list(&mut self, node: &FormalParameterList) -> Result<Vec<String>, String> {
        return match node {
            &FormalParameterList::FormalParameters(ref formal_parameters) => {
                let mut var_names: Vec<String> = vec![];
                for parameters in formal_parameters {
                    let mut other_var_names = self.visit_formal_parameters(parameters)?;
                    var_names.append(&mut other_var_names);
                }
                Ok(var_names.to_vec())
            }
        };
    }

    fn visit_formal_parameters(&mut self, node: &FormalParameters) -> Result<Vec<String>, String> {
        return match node {
            &FormalParameters::Parameters(ref names, _) => Ok(names.to_vec())
        };
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
        match node {
            &Assignment::Assign(Variable::Var(ref name), ref expression) => {
                let val = self.visit_expr(expression)?;

                self.scope()?.set(name.clone(), val);
            }
        };

        return Ok(());
    }

    fn visit_procedure_call(&mut self, node: &ProcedureCall) -> Result<(), String> {
        return match node {
            &ProcedureCall::Call(Variable::Var(ref procedure_name), ProcedureParameters::Parameters(ref given_variables)) => {
                let procedure = match self.scope()?.get(procedure_name) {
                    Some(&Object::Procedure(ref name, ref declared_params, ref block)) => Ok(Object::Procedure(name.clone(), declared_params.to_vec(), block.clone())),
                    _                                                                  => Err(String::from("Procedure Call Interpreter Error"))
                }?;

                match procedure {
                    Object::Procedure(procedure_name, declared_params, block) => {
                        self.enter_scope(procedure_name);

                        for (declared, given) in declared_params.iter().zip(given_variables.iter()) {
                            let given_variable = self.visit_variable(given)?;
                            self.scope()?.set(declared.to_owned(), given_variable);
                        }

                        self.visit_block(&block)?;
                        self.leave_scope();

                        Ok(())
                    },
                    _                                                         => Err(String::from("Procedure Call Interpreter Error"))
                }
            }
        };
    }

    fn visit_expr(&mut self, node: &Expr) -> Result<Object, String> {
        return match node {
            &Expr::BinOp(_, _, _)         => self.visit_binop(node),
            &Expr::UnaryOp(_, _)          => self.visit_unaryop(node),
            &Expr::Int(_)                 => self.visit_int(node),
            &Expr::Float(_)               => self.visit_float(node),
            &Expr::String(_)              => self.visit_string(node),
            &Expr::Variable(ref variable) => self.visit_variable(variable)
        };
    }

    fn visit_binop(&mut self, node: &Expr) -> Result<Object, String> {
        return match node {
            &Expr::BinOp(ref left, Operator::Plus, ref right)          => Ok(self.visit_expr(left)?.add(&self.visit_expr(right)?)?),
            &Expr::BinOp(ref left, Operator::Minus, ref right)         => Ok(self.visit_expr(left)?.subtract(&self.visit_expr(right)?)?),
            &Expr::BinOp(ref left, Operator::Multiply, ref right)      => Ok(self.visit_expr(left)?.multiply(&self.visit_expr(right)?)?),
            &Expr::BinOp(ref left, Operator::FloatDivide, ref right)   => Ok(self.visit_expr(left)?.float_divide(&self.visit_expr(right)?)?),
            &Expr::BinOp(ref left, Operator::IntegerDivide, ref right) => Ok(self.visit_expr(left)?.integer_divide(&self.visit_expr(right)?)?),
            _                                                          => Err(String::from("Interpreter Error"))
        };
    }

    fn visit_unaryop(&mut self, node: &Expr) -> Result<Object, String> {
        return match node {
            &Expr::UnaryOp(Operator::Plus, ref factor)  => Ok(self.visit_expr(factor)?.unary_plus()?),
            &Expr::UnaryOp(Operator::Minus, ref factor) => Ok(self.visit_expr(factor)?.unary_minus()?),
            _                                           => Err(String::from("Interpreter Error"))
        };
    }

    fn visit_int(&mut self, node: &Expr) -> Result<Object, String> {
        return match node {
            &Expr::Int(i) => Ok(Object::Primitive(Primitive::Integer(i))),
            _             => Err(String::from("Interpreter Error"))
        };
    }

    fn visit_float(&mut self, node: &Expr) -> Result<Object, String> {
        return match node {
            &Expr::Float(i) => Ok(Object::Primitive(Primitive::Float(i))),
            _               => Err(String::from("Interpreter Error"))
        };
    }

    fn visit_string(&mut self, node: &Expr) -> Result<Object, String> {
        return match node {
            &Expr::String(ref s) => Ok(Object::Primitive(Primitive::String(s.clone()))),
            _                    => Err(String::from("Interpreter Error"))
        };
    }

    fn visit_variable(&mut self, node: &Variable) -> Result<Object, String> {
        return match node {
            &Variable::Var(ref name) => {
                match self.scope()?.get(name) {
                    Some(object) => Ok(object.clone()),
                    None         => Err(String::from(format!("Unknown variable: {}", name)))
                }
            }
        }
    }

    fn enter_scope(&mut self, name: String) {
        let current_scope = self.scope.take();
        match current_scope {
            Some(scope) => self.scope = Some(Scope::with_enclosing_scope(name, scope)),
            None        => self.scope = Some(Scope::new(name))
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

    fn scope(&mut self) -> Result<&mut Scope, String> {
        return match self.scope {
            Some(ref mut scope) => Ok(scope),
            None                => Err(String::from("Unknown Scope"))
        };
    }
}
