use parser::ast::VariableDeclaration;
use parser::ast::TypeSpec;
use parser::ast::Expr;
use parser::ast::Operator;
use parser::ast::Assignment;
use parser::ast::Variable;
use parser::Parser;

use symbol_table::SymbolTable;
use symbol_table::Symbol;

use super::node_visitor::NodeVisitor;

use std::collections::HashMap;
use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt;

pub struct Interpreter<'a> {
    parser: Parser<'a>,
    global_scope: HashMap<String, f32>,
    symbol_table: SymbolTable
}

impl<'a> Display for Interpreter<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        return write!(f, "{:?}\n{:?}", self.global_scope, self.symbol_table);
    }
}

impl<'a> Interpreter<'a> {
    pub fn new(parser: Parser<'a>) -> Interpreter<'a> {
        return Interpreter {
            parser,
            global_scope: HashMap::new(),
            symbol_table: SymbolTable::new()
        };
    }

    pub fn interpret(&mut self) -> Result<(), String> {
        return match self.parser.parse() {
            Ok(program) => self.visit_program(program),
            Err(e)  => Err(e)
        }
    }
}

impl<'a> NodeVisitor for Interpreter<'a> {

    fn visit_unaryop(&mut self, expr: Expr) -> Result<f32, String> {
        return match expr {
            Expr::UnaryOp(Operator::Plus, factor)  => Ok(self.visit_expr(*factor)?),
            Expr::UnaryOp(Operator::Minus, factor) => Ok(-self.visit_expr(*factor)?),
            _                                      => Err(String::from("Interpreter Error"))
        }
    }

    fn visit_binop(&mut self, expr: Expr) -> Result<f32, String> {
        return match expr {
            Expr::BinOp(left, Operator::Plus, right)          => Ok(self.visit_expr(*left)? + self.visit_expr(*right)?),
            Expr::BinOp(left, Operator::Minus, right)         => Ok(self.visit_expr(*left)? - self.visit_expr(*right)?),
            Expr::BinOp(left, Operator::Multiply, right)      => Ok(self.visit_expr(*left)? * self.visit_expr(*right)?),
            Expr::BinOp(left, Operator::FloatDivide, right)   => Ok(self.visit_expr(*left)? / self.visit_expr(*right)?),
            Expr::BinOp(left, Operator::IntegerDivide, right) => Ok((self.visit_expr(*left)? / self.visit_expr(*right)?).round()),
            _                                                 => Err(String::from("Interpreter Error"))
        }
    }

    fn visit_num(&mut self, expr: Expr) -> Result<f32, String> {
        return match expr {
            Expr::Num(i) => Ok(i),
            _            => Err(String::from("Interpreter Error"))
        }
    }

    fn visit_assignment(&mut self, node: Assignment) -> Result<(), String> {
        match node {
            Assignment::Assign(Variable::Var(name), expression) => {
                let val = self.visit_expr(expression)?;

                let symbol = match self.symbol_table.lookup(&name) {
                    Some(symbol) => Ok(symbol),
                    None          => Err(String::from(format!("Variable {} was never defined", name)))
                }?;

                match symbol {
                    &Symbol::INTEGER(_) => match val.to_string().parse::<i32>() {
                        Ok(_) => Ok(()),
                        _     => Err(String::from("Expected INTEGER type"))
                    },
                    &Symbol::REAL(_)    => match val.to_string().parse::<f32>() {
                        Ok(_) => Ok(()),
                        _     => Err(String::from("Expected INTEGER type"))
                    },
                }?;

                self.global_scope.insert(name, val);
            }
        };

        return Ok(());
    }
    fn visit_variable(&mut self, node: Variable) -> Result<f32, String> {
        return match node {
            Variable::Var(name) => {
                match self.global_scope.get(&name) {
                    Some(&val) => Ok(val),
                    None => Err(String::from(format!("Unknown variable: {}", name)))
                }
            }
        }
    }

    fn visit_variable_declaration(&mut self, node: VariableDeclaration) -> Result<(), String> {
        match node {
            VariableDeclaration::Variables(names, TypeSpec::REAL) => {
                for name in names {
                    let symbol = Symbol::REAL(name);

                    self.symbol_table.define(symbol);
                }
            },
            VariableDeclaration::Variables(names, TypeSpec::INTEGER) => {
                for name in names {
                    let symbol = Symbol::INTEGER(name);

                    self.symbol_table.define(symbol);
                }
            }
        };

        return Ok(());
    }
}
