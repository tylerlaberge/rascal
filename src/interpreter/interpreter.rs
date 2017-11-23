use parser::ast::Program;
use parser::ast::Block;
use parser::ast::Declarations;
use parser::ast::ProcedureDeclaration;
use parser::ast::FunctionDeclaration;
use parser::ast::FormalParameterList;
use parser::ast::FormalParameters;
use parser::ast::Compound;
use parser::ast::StatementList;
use parser::ast::Statement;
use parser::ast::FunctionCall;
use parser::ast::CallParameters;
use parser::ast::Expr;
use parser::ast::UnaryOpExpr;
use parser::ast::BinaryOpExpr;
use parser::ast::GroupedExpr;
use parser::ast::Literal;
use parser::ast::Assignment;
use parser::ast::Variable;
use parser::ast::BinaryOperator;
use parser::ast::UnaryOperator;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt;

use super::scope::Scope;
use super::object::Object;
use super::object::Primitive;
use super::object::BuiltInFunction;
use super::built_ins;

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

    fn init_built_ins(&mut self) -> Result<(), String> {
        self.scope()?.set(String::from("writeln"), Object::BuiltInFunction(BuiltInFunction::WriteLn(built_ins::writeln)));

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

    fn visit_block(&mut self, node: &Block) -> Result<Object, String> {
        return match node {
            &Block::Block(ref declarations, ref compound) => {
                self.visit_declarations(declarations)?;
                let result = self.visit_compound(compound)?;

                Ok(result)
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
                &Declarations::FunctionDeclarations(ref function_declarations) => {
                    for function_declaration in function_declarations {
                        self.visit_function_declaration(function_declaration)?;
                    }
                }
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

    fn visit_function_declaration(&mut self, node: &FunctionDeclaration) -> Result<(), String> {
        return match node {
            &FunctionDeclaration::Function(ref name, ref parameter_list, ref block, ref return_type) => {
                let parameters = self.visit_formal_parameter_list(parameter_list)?;

                self.scope()?.set(name.to_owned(), Object::Function(name.to_owned(), parameters, block.clone(), return_type.clone()));

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

    fn visit_compound(&mut self, node: &Compound) -> Result<Object, String> {
        return match node {
            &Compound::StatementList(ref statement_list) => self.visit_statement_list(statement_list)
        };
    }

    fn visit_statement_list(&mut self, node: &StatementList) -> Result<Object, String> {
        return match node {
            &StatementList::Statements(ref statements) => {
                let mut result = Object::Unit;
                for statement in statements {
                    result = self.visit_statement(statement)?;
                }

                Ok(result)
            }
        };
    }

    fn visit_statement(&mut self, node: &Statement) -> Result<Object, String> {
        return match node {
            &Statement::Compound(ref compound)            => self.visit_compound(compound),
            &Statement::Assignment(ref assignment)        => self.visit_assignment(assignment),
            &Statement::FunctionCall(ref function_call)   => self.visit_function_call(function_call),
            &Statement::Empty                             => Ok(Object::Unit)
        };
    }

    fn visit_assignment(&mut self, node: &Assignment) -> Result<Object, String> {
        return match node {
            &Assignment::Assign(Variable::Var(ref name), ref expression) => {
                let val = self.visit_expr(expression)?;
                self.scope()?.set(name.clone(), val.clone());

                Ok(val)
            }
        };
    }

    fn visit_function_call(&mut self, node: &FunctionCall) -> Result<Object, String> {
        return match node {
            &FunctionCall::Call(Variable::Var(ref function_name), CallParameters::Parameters(ref given_parameters)) => {
                let callable = match self.scope()?.get(function_name) {
                    Some(&Object::Function(ref name, ref declared_params, ref block, ref return_type))  => Ok(Object::Function(name.clone(), declared_params.to_vec(), block.clone(), return_type.clone())),
                    Some(&Object::Procedure(ref name, ref declared_params, ref block))                  => Ok(Object::Procedure(name.clone(), declared_params.to_vec(), block.clone())),
                    Some(&Object::BuiltInFunction(ref func))                                            => Ok(Object::BuiltInFunction(func.clone())),
                    _                                                                                   => Err(String::from("Callable Interpreter Error"))
                }?;

                match callable {
                    Object::Function(function_name, declared_params, block, _) => {
                        self.enter_scope(function_name);

                        for (declared, given) in declared_params.iter().zip(given_parameters.iter()) {
                            let given_parameter = self.visit_expr(given)?;
                            self.scope()?.set(declared.to_owned(), given_parameter);
                        }

                        let result = self.visit_block(&block)?;
                        self.leave_scope();

                        Ok(result)
                    },
                    Object::Procedure(procedure_name, declared_params, block) => {
                        self.enter_scope(procedure_name);

                        for (declared, given) in declared_params.iter().zip(given_parameters.iter()) {
                            let given_parameter = self.visit_expr(given)?;
                            self.scope()?.set(declared.to_owned(), given_parameter);
                        }

                        self.visit_block(&block)?;
                        self.leave_scope();

                        Ok(Object::Unit)
                    },
                    Object::BuiltInFunction(BuiltInFunction::WriteLn(func)) => {
                        if given_parameters.len() != 1 {
                            Err(String::from("Built in function writeln expected 1 parameter"))
                        } else {
                            let parameter = self.visit_expr(&given_parameters[0])?;
                            match parameter {
                                Object::Primitive(Primitive::String(text)) => Ok(func(text)),
                                _                                          => Err(String::from("Built in function writeln expected String parameter"))
                            }
                        }
                    },
                    _                                                       => Err(String::from("Procedure Call Interpreter Error"))
                }
            }
        };
    }

    fn visit_expr(&mut self, node: &Expr) -> Result<Object, String> {
        return match node {
            &Expr::UnaryOp(ref unaryop_expr)       => self.visit_unaryop(unaryop_expr),
            &Expr::BinOp(ref binop_expr)           => self.visit_binop(binop_expr),
            &Expr::Group(ref group_expr)           => self.visit_group(group_expr),
            &Expr::Literal(ref literal)            => self.visit_literal(literal),
            &Expr::Variable(ref variable)          => self.visit_variable(variable),
            &Expr::FunctionCall(ref function_call) => self.visit_function_call(function_call)
        };
    }

    fn visit_unaryop(&mut self, node: &UnaryOpExpr) -> Result<Object, String> {
        return match node {
            &UnaryOpExpr::UnaryOp(UnaryOperator::Plus, ref factor)  => Ok(self.visit_expr(factor)?.unary_plus()?),
            &UnaryOpExpr::UnaryOp(UnaryOperator::Minus, ref factor) => Ok(self.visit_expr(factor)?.unary_minus()?),
        };
    }

    fn visit_binop(&mut self, node: &BinaryOpExpr) -> Result<Object, String> {
        return match node {
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::Plus, ref right)          => Ok(self.visit_expr(left)?.add(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::Minus, ref right)         => Ok(self.visit_expr(left)?.subtract(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::Multiply, ref right)      => Ok(self.visit_expr(left)?.multiply(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::FloatDivide, ref right)   => Ok(self.visit_expr(left)?.float_divide(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::IntegerDivide, ref right) => Ok(self.visit_expr(left)?.integer_divide(&self.visit_expr(right)?)?),
        };
    }

    fn visit_group(&mut self, node: &GroupedExpr) -> Result<Object, String> {
        return match node {
            &GroupedExpr::Group(ref expr) => self.visit_expr(expr)
        };
    }

    fn visit_literal(&mut self, node: &Literal) -> Result<Object, String> {
        return match node {
            &Literal::Int(i)        => Ok(Object::Primitive(Primitive::Integer(i))),
            &Literal::Float(f)      => Ok(Object::Primitive(Primitive::Float(f))),
            &Literal::String(ref s) => Ok(Object::Primitive(Primitive::String(s.clone())))
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
        };
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
