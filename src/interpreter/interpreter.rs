use std::fmt;
use std::fmt::{Display, Formatter};
use parser::ast::{
    Program,
    Block,
    Declarations,
    ProcedureDeclaration,
    FunctionDeclaration,
    FormalParameterList,
    FormalParameters,
    Compound,
    Statement,
    IfStatement,
    Assignment,
    Variable,
    FunctionCall,
    CallParameters,
    Expr,
    BinaryOpExpr,
    BinaryOperator,
    UnaryOpExpr,
    UnaryOperator,
    GroupedExpr,
    Literal
};
use super::object::{Object, Primitive, BuiltInFunction};
use super::scope::Scope;
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
        self.visit_program(program).unwrap();
    }

    fn init_built_ins(&mut self) -> Result<(), String> {
        self.scope()?.set(String::from("write"), Object::BuiltInFunction(BuiltInFunction::Write(built_ins::write)));
        self.scope()?.set(String::from("writeln"), Object::BuiltInFunction(BuiltInFunction::WriteLn(built_ins::writeln)));
        self.scope()?.set(String::from("readln"), Object::BuiltInFunction(BuiltInFunction::ReadLn(built_ins::readln)));
        self.scope()?.set(String::from("IntToString"), Object::BuiltInFunction(BuiltInFunction::IntToString(built_ins::int_to_string)));
        self.scope()?.set(String::from("RealToString"), Object::BuiltInFunction(BuiltInFunction::RealToString(built_ins::real_to_string)));
        self.scope()?.set(String::from("StringToInt"), Object::BuiltInFunction(BuiltInFunction::StringToInt(built_ins::string_to_int)));
        self.scope()?.set(String::from("StringToReal"), Object::BuiltInFunction(BuiltInFunction::StringToReal(built_ins::string_to_real)));

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

    pub fn visit_block(&mut self, node: &Block) -> Result<Object, String> {
        return match node {
            &Block::Block(ref declarations, ref compound) => {
                self.visit_declarations(declarations)?;
                let result = self.visit_compound(compound)?;

                Ok(result)
            }
        };
    }

    pub fn visit_declarations(&mut self, node: &Vec<Declarations>) -> Result<(), String> {
        for declarations in node {
            match declarations {
                &Declarations::ProcedureDeclarations(ref procedure_declarations) => {
                    for procedure_declaration in procedure_declarations {
                        self.visit_procedure_declaration(procedure_declaration)?;
                    }
                },
                &Declarations::FunctionDeclarations(ref function_declarations)   => {
                    for function_declaration in function_declarations {
                        self.visit_function_declaration(function_declaration)?;
                    }
                }
                _                                                                => ()
            };
        }
        return Ok(());
    }

    pub fn visit_procedure_declaration(&mut self, node: &ProcedureDeclaration) -> Result<(), String> {
        return match node {
            &ProcedureDeclaration::Procedure(ref name, ref parameter_list, ref block) => {
                let parameters = self.visit_formal_parameter_list(parameter_list)?;

                self.scope()?.set(name.to_owned(), Object::Procedure(name.to_owned(), parameters, block.clone()));

                Ok(())
            }
        };
    }

    pub fn visit_function_declaration(&mut self, node: &FunctionDeclaration) -> Result<(), String> {
        return match node {
            &FunctionDeclaration::Function(ref name, ref parameter_list, ref block, ref return_type) => {
                let parameters = self.visit_formal_parameter_list(parameter_list)?;

                self.scope()?.set(name.to_owned(), Object::Function(name.to_owned(), parameters, block.clone(), return_type.clone()));

                Ok(())
            }
        };
    }

    pub fn visit_formal_parameter_list(&mut self, node: &FormalParameterList) -> Result<Vec<String>, String> {
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

    pub fn visit_formal_parameters(&mut self, node: &FormalParameters) -> Result<Vec<String>, String> {
        return match node {
            &FormalParameters::Parameters(ref names, _) => Ok(names.to_vec())
        };
    }

    pub fn visit_compound(&mut self, node: &Compound) -> Result<Object, String> {
        return match node {
            &Compound::Statements(ref statements) => {
                let mut result = Object::Unit;
                for statement in statements {
                    result = self.visit_statement(statement)?;
                }

                Ok(result)
            }
        };
    }

    pub fn visit_statement(&mut self, node: &Statement) -> Result<Object, String> {
        return match node {
            &Statement::Compound(ref compound)          => self.visit_compound(compound),
            &Statement::Assignment(ref assignment)      => self.visit_assignment(assignment),
            &Statement::IfStatement(ref if_statement)   => self.visit_if_statement(if_statement),
            &Statement::FunctionCall(ref function_call) => self.visit_function_call(function_call),
        };
    }

    pub fn visit_if_statement(&mut self, node: &IfStatement) -> Result<Object, String> {
        return match node {
            &IfStatement::If(ref expr, ref compound_statement) => match self.visit_expr(expr)? {
                Object::Primitive(Primitive::Boolean(true)) => {
                    self.visit_compound(compound_statement)?;
                    Ok(Object::Unit)
                },
                Object::Primitive(Primitive::Boolean(false)) => Ok(Object::Unit),
                _                                            => Err(String::from("Internal Interpreter Error: If Statement evaluated to non-boolean type"))
            },
            &IfStatement::IfElse(ref expr, ref if_compound_statement, ref else_compound_statement) => match self.visit_expr(expr)? {
                Object::Primitive(Primitive::Boolean(true)) => {
                    self.visit_compound(if_compound_statement)?;
                    Ok(Object::Unit)
                },
                Object::Primitive(Primitive::Boolean(false)) => {
                    self.visit_compound(else_compound_statement)?;
                    Ok(Object::Unit)
                },
                _                                            => Err(String::from("Internal Interpreter Error: If Statement evaluated to non-boolean type"))
            },
            &IfStatement::IfElseIf(ref expr, ref if_compound_statement, ref else_if_statement) => match self.visit_expr(expr)? {
                Object::Primitive(Primitive::Boolean(true)) => {
                    self.visit_compound(if_compound_statement)?;
                    Ok(Object::Unit)
                },
                Object::Primitive(Primitive::Boolean(false)) => self.visit_if_statement(else_if_statement),
                _                                            => Err(String::from("Internal Interpreter Error: If Statement evaluated to non-boolean type"))
            },
        }
    }

    pub fn visit_assignment(&mut self, node: &Assignment) -> Result<Object, String> {
        return match node {
            &Assignment::Assign(Variable::Var(ref name), ref expression) => {
                let val = self.visit_expr(expression)?;
                self.scope()?.set(name.clone(), val.clone());

                Ok(val)
            }
        };
    }

    pub fn visit_function_call(&mut self, node: &FunctionCall) -> Result<Object, String> {
        return match node {
            &FunctionCall::Call(Variable::Var(ref function_name), CallParameters::Parameters(ref given_parameters)) => {
                let callable = match self.scope()?.get(function_name) {
                    Some(&Object::Function(ref name, ref declared_params, ref block, ref return_type))  => Ok(Object::Function(name.clone(), declared_params.to_vec(), block.clone(), return_type.clone())),
                    Some(&Object::Procedure(ref name, ref declared_params, ref block))                  => Ok(Object::Procedure(name.clone(), declared_params.to_vec(), block.clone())),
                    Some(&Object::BuiltInFunction(ref func))                                            => Ok(Object::BuiltInFunction(func.clone())),
                    _                                                                                   => Err(String::from(format!("Internal Interpreter Error: Expected a callable with the name '{:?}'", function_name)))
                }?;

                match callable {
                    Object::Procedure(name, declared_params, block)
                    | Object::Function(name, declared_params, block, _) => {
                        self.enter_scope(name);

                        for (declared, given) in declared_params.iter().zip(given_parameters.iter()) {
                            let given_parameter = self.visit_expr(given)?;
                            self.scope()?.set(declared.to_owned(), given_parameter);
                        }

                        let result = self.visit_block(&block)?;
                        self.leave_scope();

                        Ok(result)
                    },
                    Object::BuiltInFunction(built_in_function) => match built_in_function {
                        BuiltInFunction::Write(func)        => match self.visit_expr(&given_parameters[0])? {
                            Object::Primitive(Primitive::String(text)) => Ok(func(text)?),
                            _                                          => Err(String::from("Internal Interpreter Error: Built in function 'write' expected String parameter"))
                        },
                        BuiltInFunction::WriteLn(func)      => match self.visit_expr(&given_parameters[0])? {
                            Object::Primitive(Primitive::String(text)) => Ok(func(text)?),
                            _                                          => Err(String::from("Internal Interpreter Error: Built in function 'writeln' expected String parameter"))
                        },
                        BuiltInFunction::ReadLn(func)       => Ok(func()?),
                        BuiltInFunction::IntToString(func)  => match self.visit_expr(&given_parameters[0])? {
                            Object::Primitive(Primitive::Integer(value)) => Ok(func(value)?),
                            _                                            => Err(String::from("Internal Interpreter Error: Built in function 'IntToString' expected Integer parameter"))
                        },
                        BuiltInFunction::RealToString(func) => match self.visit_expr(&given_parameters[0])? {
                            Object::Primitive(Primitive::Float(value)) => Ok(func(value)?),
                            _                                          => Err(String::from("Internal Interpreter Error: Built in function 'RealToString' expected Real parameter"))
                        },
                        BuiltInFunction::StringToInt(func)  => match self.visit_expr(&given_parameters[0])? {
                            Object::Primitive(Primitive::String(text)) => Ok(func(text)?),
                            _                                          => Err(String::from("Internal Interpreter Error: Built in function 'StringToInt' expected String parameter"))
                        },
                        BuiltInFunction::StringToReal(func) => match self.visit_expr(&given_parameters[0])? {
                            Object::Primitive(Primitive::String(text)) => Ok(func(text)?),
                            _                                          => Err(String::from("Internal Interpreter Error: Built in function 'StringToReal' expected String parameter"))
                        }
                    },
                    _                                          => Err(String::from(format!("Internal Interpreter Error: Expected {:?} to be a callable", callable)))
                }
            }
        };
    }

    pub fn visit_expr(&mut self, node: &Expr) -> Result<Object, String> {
        return match node {
            &Expr::UnaryOp(ref unaryop_expr)       => self.visit_unaryop(unaryop_expr),
            &Expr::BinOp(ref binop_expr)           => self.visit_binop(binop_expr),
            &Expr::Group(ref group_expr)           => self.visit_group(group_expr),
            &Expr::Literal(ref literal)            => self.visit_literal(literal),
            &Expr::Variable(ref variable)          => self.visit_variable(variable),
            &Expr::FunctionCall(ref function_call) => self.visit_function_call(function_call)
        };
    }

    pub fn visit_unaryop(&mut self, node: &UnaryOpExpr) -> Result<Object, String> {
        return match node {
            &UnaryOpExpr::UnaryOp(UnaryOperator::Plus, ref expr)  => Ok(self.visit_expr(expr)?.unary_plus()?),
            &UnaryOpExpr::UnaryOp(UnaryOperator::Minus, ref expr) => Ok(self.visit_expr(expr)?.unary_minus()?),
            &UnaryOpExpr::UnaryOp(UnaryOperator::Not, ref expr)   => Ok(self.visit_expr(expr)?.negate()?)
        };
    }

    pub fn visit_binop(&mut self, node: &BinaryOpExpr) -> Result<Object, String> {
        return match node {
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::Plus, ref right)               => Ok(self.visit_expr(left)?.add(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::Minus, ref right)              => Ok(self.visit_expr(left)?.subtract(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::Multiply, ref right)           => Ok(self.visit_expr(left)?.multiply(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::FloatDivide, ref right)        => Ok(self.visit_expr(left)?.float_divide(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::IntegerDivide, ref right)      => Ok(self.visit_expr(left)?.integer_divide(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::And, ref right)                => Ok(self.visit_expr(left)?.and(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::Or, ref right)                 => Ok(self.visit_expr(left)?.or(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::LessThan, ref right)           => Ok(self.visit_expr(left)?.less_than(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::LessThanOrEqual, ref right)    => Ok(self.visit_expr(left)?.less_than_or_equal(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::GreaterThan, ref right)        => Ok(self.visit_expr(left)?.greater_than(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::GreaterThanOrEqual, ref right) => Ok(self.visit_expr(left)?.greater_than_or_equal(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::Equal, ref right)              => Ok(self.visit_expr(left)?.equal(&self.visit_expr(right)?)?),
            &BinaryOpExpr::BinaryOp(ref left, BinaryOperator::NotEqual, ref right)           => Ok(self.visit_expr(left)?.not_equal(&self.visit_expr(right)?)?),
        };
    }

    pub fn visit_group(&mut self, node: &GroupedExpr) -> Result<Object, String> {
        return match node {
            &GroupedExpr::Group(ref expr) => self.visit_expr(expr)
        };
    }

    pub fn visit_literal(&mut self, node: &Literal) -> Result<Object, String> {
        return match node {
            &Literal::Int(i)        => Ok(Object::Primitive(Primitive::Integer(i))),
            &Literal::Float(f)      => Ok(Object::Primitive(Primitive::Float(f))),
            &Literal::String(ref s) => Ok(Object::Primitive(Primitive::String(s.clone()))),
            &Literal::Boolean(b)    => Ok(Object::Primitive(Primitive::Boolean(b)))
        };
    }

    pub fn visit_variable(&mut self, node: &Variable) -> Result<Object, String> {
        return match node {
            &Variable::Var(ref name) => {
                match self.scope()?.get(name) {
                    Some(object) => Ok(object.clone()),
                    None         => Err(String::from(format!("Internal Interpreter Error: Unknown variable '{}'", name)))
                }
            }
        };
    }

    pub fn enter_scope(&mut self, name: String) {
        let current_scope = self.scope.take();
        match current_scope {
            Some(scope) => self.scope = Some(Scope::with_enclosing_scope(name, scope)),
            None        => self.scope = Some(Scope::new(name))
        };
    }

    pub fn leave_scope(&mut self) {
        let current_scope = self.scope.take();

        match current_scope {
            Some(scope) => self.scope = scope.enclosing_scope(),
            None        => self.scope = None
        };
    }

    pub fn scope(&mut self) -> Result<&mut Scope, String> {
        return match self.scope {
            Some(ref mut scope) => Ok(scope),
            None                => Err(String::from("Internal Interpreter Error: Unknown Scope"))
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use parser::ast::TypeSpec;

    #[test]
    fn visit_program() {
        let mut interpreter = Interpreter::new();
        let program = Program::Program(
            Variable::Var(String::from("test")),
            Block::Block(vec![], Compound::Statements(vec![]))
        );

        assert_eq!(interpreter.visit_program(&program), Ok(()));
    }

    #[test]
    fn visit_block() {
        let mut interpreter = Interpreter::new();
        let block = Block::Block(vec![], Compound::Statements(vec![]));

        assert_eq!(interpreter.visit_block(&block), Ok(Object::Unit));
    }

    #[test]
    fn visit_declarations() {
        let mut interpreter = Interpreter::new();
        let declarations = vec![
            Declarations::ProcedureDeclarations(vec![]),
            Declarations::FunctionDeclarations(vec![]),
            Declarations::VariableDeclarations(vec![])
        ];

        assert_eq!(interpreter.visit_declarations(&declarations), Ok(()));
    }

    #[test]
    fn visit_procedure_declaration() {
        let mut interpreter = Interpreter::new();
        interpreter.enter_scope(String::from("test_scope"));

        let procedure = ProcedureDeclaration::Procedure(
            String::from("test"),
            FormalParameterList::FormalParameters(vec![]),
            Block::Block(vec![], Compound::Statements(vec![]))
        );

        assert_eq!(interpreter.visit_procedure_declaration(&procedure), Ok(()));
        assert_eq!(
            interpreter.scope().unwrap().get(&String::from("test")),
            Some(
                &Object::Procedure(
                    String::from("test"),
                    vec![],
                    Block::Block(vec![], Compound::Statements(vec![]))
                )
            )
        );
    }

    #[test]
    fn visit_function_declaration() {
        let mut interpreter = Interpreter::new();
        interpreter.enter_scope(String::from("test_scope"));

        let function = FunctionDeclaration::Function(
            String::from("test"),
            FormalParameterList::FormalParameters(vec![]),
            Block::Block(vec![], Compound::Statements(vec![])),
            TypeSpec::INTEGER
        );

        assert_eq!(interpreter.visit_function_declaration(&function), Ok(()));
        assert_eq!(
            interpreter.scope().unwrap().get(&String::from("test")),
            Some(
                &Object::Function(
                    String::from("test"),
                    vec![],
                    Block::Block(vec![], Compound::Statements(vec![])),
                    TypeSpec::INTEGER
                )
            )
        );
    }

    #[test]
    fn visit_formal_parameter_list() {
        let mut interpreter = Interpreter::new();
        let formal_parameter_list = FormalParameterList::FormalParameters(vec![
            FormalParameters::Parameters(
                vec![String::from("int_one"), String::from("int_two")], TypeSpec::INTEGER
            ),
            FormalParameters::Parameters(
                vec![String::from("bool_one")], TypeSpec::BOOLEAN
            ),
            FormalParameters::Parameters(
                vec![String::from("real_one"), String::from("real_two"), String::from("real_three")], TypeSpec::REAL
            )
        ]);

        assert_eq!(
            interpreter.visit_formal_parameter_list(&formal_parameter_list),
            Ok(vec![
                String::from("int_one"), String::from("int_two"), String::from("bool_one"),
                String::from("real_one"), String::from("real_two"), String::from("real_three")
            ])
        );
    }

    #[test]
    fn visit_formal_parameters() {
        let mut interpreter = Interpreter::new();
        let formal_parameters = FormalParameters::Parameters(
            vec![String::from("foo"), String::from("bar")],
            TypeSpec::INTEGER
        );

        assert_eq!(
            interpreter.visit_formal_parameters(&formal_parameters),
            Ok(vec![String::from("foo"), String::from("bar")])
        );
    }

    #[test]
    fn visit_compound() {
        let mut interpreter = Interpreter::new();
        interpreter.enter_scope(String::from("test_scope"));

        let compound = Compound::Statements(vec![
            Statement::Assignment(Assignment::Assign(Variable::Var(String::from("foo")), Expr::Literal(Literal::Int(5)))),
            Statement::Assignment(Assignment::Assign(Variable::Var(String::from("bar")), Expr::Literal(Literal::Boolean(true)))),
        ]);

        assert_eq!(
            interpreter.visit_compound(&compound),
            Ok(Object::Primitive(Primitive::Boolean(true)))
        );
        assert_eq!(
            interpreter.scope().unwrap().get(&String::from("foo")),
            Some(&Object::Primitive(Primitive::Integer(5)))
        );
        assert_eq!(
            interpreter.scope().unwrap().get(&String::from("bar")),
            Some(&Object::Primitive(Primitive::Boolean(true)))
        );
    }

    #[test]
    fn visit_if_statement() {
        let mut interpreter = Interpreter::new();
        let if_statement = IfStatement::IfElseIf(             // if false then
            Expr::Literal(Literal::Boolean(false)),        //     begin
            Compound::Statements(vec![]),                  //     end
            Box::new(                                      // else if true then
                IfStatement::IfElse(                       //     begin
                    Expr::Literal(Literal::Boolean(true)), //     end
                    Compound::Statements(vec![]),          // else
                    Compound::Statements(vec![]))          //     begin
            )                                                 //     end
        );

        assert_eq!(interpreter.visit_if_statement(&if_statement), Ok(Object::Unit));
    }

    #[test]
    fn visit_assignment() {
        let mut interpreter = Interpreter::new();
        interpreter.enter_scope(String::from("test_scope"));

        let assignment = Assignment::Assign(
            Variable::Var(String::from("test")),
            Expr::Literal(Literal::Int(5))
        );

        assert_eq!(interpreter.visit_assignment(&assignment), Ok(Object::Primitive(Primitive::Integer(5))));
        assert_eq!(
            interpreter.scope().unwrap().get(&String::from("test")),
            Some(&Object::Primitive(Primitive::Integer(5)))
        )
    }

    #[test]
    fn visit_function_call() {
        let mut interpreter = Interpreter::new();
        interpreter.enter_scope(String::from("test_scope"));
        interpreter.scope().unwrap().set(
            String::from("test_function"),
            Object::Function(
                String::from("test_function"),
                vec![],
                Block::Block(
                    vec![],
                    Compound::Statements(vec![
                        Statement::Assignment(
                            Assignment::Assign(
                                Variable::Var(String::from("foo")),
                                Expr::Literal(Literal::Int(5))
                            )
                        )
                    ])
                ),
                TypeSpec::INTEGER
            )
        );

        let function_call = FunctionCall::Call(
            Variable::Var(String::from("test_function")),
            CallParameters::Parameters(vec![])
        );

        assert_eq!(
            interpreter.visit_function_call(&function_call),
            Ok(Object::Primitive(Primitive::Integer(5)))
        )
    }

    #[test]
    fn visit_unaryop() {
        let mut interpreter = Interpreter::new();
        let unaryop_expr = UnaryOpExpr::UnaryOp(
            UnaryOperator::Minus,
            Expr::Literal(Literal::Int(5))
        );

        assert_eq!(
            interpreter.visit_unaryop(&unaryop_expr),
            Ok(Object::Primitive(Primitive::Integer(-5)))
        );
    }

    #[test]
    fn visit_binop() {
        let mut interpreter = Interpreter::new();
        let binop_expr = BinaryOpExpr::BinaryOp(
            Expr::Literal(Literal::Int(5)),
            BinaryOperator::Multiply,
            Expr::Literal(Literal::Int(10))
        );

        assert_eq!(
            interpreter.visit_binop(&binop_expr),
            Ok(Object::Primitive(Primitive::Integer(50)))
        );
    }

    #[test]
    fn visit_group() {
        let mut interpreter = Interpreter::new();
        let grouped_expr = GroupedExpr::Group(Expr::Literal(Literal::Int(5)));

        assert_eq!(
            interpreter.visit_group(&grouped_expr),
            Ok(Object::Primitive(Primitive::Integer(5)))
        );
    }

    #[test]
    fn visit_literal() {
        let mut interpreter = Interpreter::new();
        let literal = Literal::Boolean(true);

        assert_eq!(
            interpreter.visit_literal(&literal),
            Ok(Object::Primitive(Primitive::Boolean(true)))
        );
    }

    #[test]
    fn visit_variable() {
        let mut interpreter = Interpreter::new();
        interpreter.enter_scope(String::from("test_scope"));
        interpreter.scope().unwrap().set(
            String::from("test_var"),
            Object::Primitive(Primitive::Float(5.5))
        );

        let variable = Variable::Var(String::from("test_var"));

        assert_eq!(
            interpreter.visit_variable(&variable),
            Ok(Object::Primitive(Primitive::Float(5.5)))
        );
    }
}
