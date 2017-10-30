use parser::ast::Program;
use parser::ast::Block;
use parser::ast::Declarations;
use parser::ast::VariableDeclaration;
use parser::ast::Compound;
use parser::ast::StatementList;
use parser::ast::Statement;
use parser::ast::Assignment;
use parser::ast::Expr;
use parser::ast::Variable;

pub trait NodeVisitor {

    fn visit_variable(&mut self, node: Variable) -> Result<f32, String>;
    fn visit_unaryop(&mut self, node: Expr) -> Result<f32, String>;
    fn visit_binop(&mut self, node: Expr) -> Result<f32, String>;
    fn visit_num(&mut self, node: Expr) -> Result<f32, String>;
    fn visit_expr(&mut self, node: Expr) -> Result<f32, String> {
        return match node {
            Expr::UnaryOp(_, _)      => self.visit_unaryop(node),
            Expr::BinOp(_, _, _)     => self.visit_binop(node),
            Expr::Num(_)             => self.visit_num(node),
            Expr::Variable(variable) => self.visit_variable(variable)
        }
    }

    fn visit_assignment(&mut self, node: Assignment) -> Result<(), String>;
    fn visit_statement(&mut self, node: Statement) -> Result<(), String> {
        return match node {
            Statement::Compound(compound)     => self.visit_compound(compound),
            Statement::Assignment(assignment) => self.visit_assignment(assignment),
            Statement::Empty                  => Ok(())
        };
    }
    fn visit_statement_list(&mut self, node: StatementList) -> Result<(), String> {
        match node {
            StatementList::Statements(statements) => {
                for statement in statements {
                    self.visit_statement(statement)?;
                }
            }
        };

        return Ok(());
    }

    fn visit_compound(&mut self, node: Compound) -> Result<(), String> {
        return match node {
            Compound::StatementList(statement_list) => self.visit_statement_list(statement_list)
        };
    }

    fn visit_variable_declaration(&mut self, node: VariableDeclaration) -> Result<(), String>;

    fn visit_declarations(&mut self, node: Declarations) -> Result<(), String> {
        return match node {
            Declarations::VariableDeclarations(variable_declarations) => {
                for variable_declaration in variable_declarations {
                    self.visit_variable_declaration(variable_declaration)?;
                }

                Ok(())
            },
            Declarations::Empty => Ok(())
        }
    }

    fn visit_block(&mut self, node: Block) -> Result<(), String> {
        return match node {
            Block::Block(declarations, compound) => {
                self.visit_declarations(declarations)?;
                self.visit_compound(compound)?;

                Ok(())
            }
        }
    }

    fn visit_program(&mut self, node: Program) -> Result<(), String> {
        return match node {
            Program::Program(_, block) => self.visit_block(block)
        };
    }
}