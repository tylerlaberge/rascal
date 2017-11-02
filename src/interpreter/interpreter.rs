use parser::ast::Program;
use parser::ast::Block;
use parser::ast::Compound;
use parser::ast::StatementList;
use parser::ast::Statement;
use parser::ast::Expr;
use parser::ast::Assignment;
use parser::ast::Variable;
use parser::ast::Operator;

use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt;

pub struct Interpreter {
    global_scope: HashMap<String, f32>,
}

impl Display for Interpreter {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        return write!(f, "{:?}", self.global_scope);
    }
}

impl Interpreter {
    pub fn new() -> Interpreter {
        return Interpreter {
            global_scope: HashMap::new(),
        };
    }

    pub fn interpret(&mut self, program: &Program) {
        match self.visit_program(program) {
            Ok(()) => println!("{}", self),
            Err(e) => panic!("{}", e)
        };
    }

    fn visit_program(&mut self, node: &Program) -> Result<(), String> {
        return match node {
            &Program::Program(_, ref block) => self.visit_block(block)
        };
    }

    fn visit_block(&mut self, node: &Block) -> Result<(), String> {
        return match node {
            &Block::Block(_, ref compound) => self.visit_compound(compound)
        }
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
            &Statement::Compound(ref compound)     => self.visit_compound(compound),
            &Statement::Assignment(ref assignment) => self.visit_assignment(assignment),
            &Statement::Empty                      => Ok(())
        };
    }

    fn visit_assignment(&mut self, node: &Assignment) -> Result<(), String> {
        match node {
            &Assignment::Assign(Variable::Var(ref name), ref expression) => {
                let val = self.visit_expr(expression)?;

                self.global_scope.insert(name.clone(), val);
            }
        };

        return Ok(());
    }

    fn visit_expr(&mut self, node: &Expr) -> Result<f32, String> {
        return match node {
            &Expr::BinOp(_, _, _)         => self.visit_binop(node),
            &Expr::UnaryOp(_, _)          => self.visit_unaryop(node),
            &Expr::Int(_)                 => self.visit_int(node),
            &Expr::Float(_)               => self.visit_float(node),
            &Expr::Variable(ref variable) => self.visit_variable(variable)
        };
    }

    fn visit_binop(&mut self, node: &Expr) -> Result<f32, String> {
        return match node {
            &Expr::BinOp(ref left, Operator::Plus, ref right)          => Ok(self.visit_expr(left)? + self.visit_expr(right)?),
            &Expr::BinOp(ref left, Operator::Minus, ref right)         => Ok(self.visit_expr(left)? - self.visit_expr(right)?),
            &Expr::BinOp(ref left, Operator::Multiply, ref right)      => Ok(self.visit_expr(left)? * self.visit_expr(right)?),
            &Expr::BinOp(ref left, Operator::FloatDivide, ref right)   => Ok(self.visit_expr(left)? / self.visit_expr(right)?),
            &Expr::BinOp(ref left, Operator::IntegerDivide, ref right) => Ok((self.visit_expr(left)? / self.visit_expr(right)?).round()),
            _                                                          => Err(String::from("Interpreter Error"))
        };
    }

    fn visit_unaryop(&mut self, node: &Expr) -> Result<f32, String> {
        return match node {
            &Expr::UnaryOp(Operator::Plus, ref factor)  => Ok(self.visit_expr(factor)?),
            &Expr::UnaryOp(Operator::Minus, ref factor) => Ok(-self.visit_expr(factor)?),
            _                                           => Err(String::from("Interpreter Error"))
        };
    }

    fn visit_int(&mut self, node: &Expr) -> Result<f32, String> {
        return match node {
            &Expr::Int(i) => Ok(i as f32),
            _             => Err(String::from("Interpreter Error"))
        };
    }

    fn visit_float(&mut self, node: &Expr) -> Result<f32, String> {
        return match node {
            &Expr::Float(i) => Ok(i),
            _               => Err(String::from("Interpreter Error"))
        };
    }

    fn visit_variable(&mut self, node: &Variable) -> Result<f32, String> {
        return match node {
            &Variable::Var(ref name) => {
                match self.global_scope.get(name) {
                    Some(val) => Ok(val.clone()),
                    None      => Err(String::from(format!("Unknown variable: {}", name)))
                }
            }
        }
    }
}
