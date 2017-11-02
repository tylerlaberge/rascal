use parser::ast::Program;
use parser::ast::Block;
use parser::ast::Declarations;
use parser::ast::VariableDeclaration;
use parser::ast::Compound;
use parser::ast::StatementList;
use parser::ast::Statement;
use parser::ast::TypeSpec;
use parser::ast::Expr;
use parser::ast::Assignment;
use parser::ast::Variable;

use symbol_table::SymbolTable;
use symbol_table::Symbol;

use std::fmt::Display;
use std::fmt::Formatter;
use std::fmt;

pub struct SemanticAnalyzer {
    symbol_table: SymbolTable
}

impl Display for SemanticAnalyzer {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        return write!(f, "{:?}", self.symbol_table);
    }
}

impl SemanticAnalyzer {

    pub fn new() -> SemanticAnalyzer {
        return SemanticAnalyzer { symbol_table: SymbolTable::new() };
    }

    pub fn analyze(&mut self, program: &Program) {
        match self.visit_program(program) {
            Ok(()) => println!("{}", self),
            Err(e) => panic!("{}", e)
        }
    }

    fn visit_program(&mut self, node: &Program) -> Result<(), String> {
        return match node {
            &Program::Program(_, ref block) => self.visit_block(block)
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

    fn visit_declarations(&mut self, node: &Declarations) -> Result<(), String> {
        return match node {
            &Declarations::VariableDeclarations(ref variable_declarations) => {
                for variable_declaration in variable_declarations {
                    self.visit_variable_declaration(variable_declaration)?;
                }

                Ok(())
            },
            &Declarations::Empty => Ok(())
        }
    }

    fn visit_variable_declaration(&mut self, node: &VariableDeclaration) -> Result<(), String> {
        match node {
            &VariableDeclaration::Variables(ref names, TypeSpec::REAL) => {
                for name in names {
                    let symbol = Symbol::REAL(name.to_owned());

                    self.symbol_table.define(symbol);
                }
            },
            &VariableDeclaration::Variables(ref names, TypeSpec::INTEGER) => {
                for name in names {
                    let symbol = Symbol::INTEGER(name.to_owned());

                    self.symbol_table.define(symbol);
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
            &Statement::Compound(ref compound)     => self.visit_compound(compound),
            &Statement::Assignment(ref assignment) => self.visit_assignment(assignment),
            &Statement::Empty                      => Ok(())
        };
    }

    fn visit_assignment(&mut self, node: &Assignment) -> Result<(), String> {
        return match node {
            &Assignment::Assign(Variable::Var(ref name), ref expression) => {
                let symbol = match self.symbol_table.lookup(name) {
                    Some(symbol)  => Ok(symbol.clone()),
                    None          => Err(String::from(format!("Variable {} was never defined", name)))
                }?;

                match symbol {
                    Symbol::INTEGER(_) =>
                        match self.visit_expr(expression)? {
                            TypeSpec::INTEGER  => Ok(()),
                            _                  => Err(String::from("Can't assign mismatched types"))
                        },
                    Symbol::REAL(_)    =>
                        match self.visit_expr(expression)? {
                            TypeSpec::REAL => Ok(()),
                            _              => Err(String::from("Can't assign mismatched types"))
                        }
                }
            }
        };
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
                match self.symbol_table.lookup(name) {
                    Some(symbol) => match symbol {
                        &Symbol::INTEGER(_) => Ok(TypeSpec::INTEGER),
                        &Symbol::REAL(_)    => Ok(TypeSpec::REAL)
                    },
                    None         => Err(String::from(format!("Unknown variable: {}", name)))
                }
            }
        }
    }
}