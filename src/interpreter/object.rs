use parser::ast::Block;
use parser::ast::TypeSpec;

use std::fmt::Debug;
use std::fmt;

#[derive(Clone)]
pub enum Object {
    Unit,
    Primitive(Primitive),
    Procedure(String, Vec<String>, Block),
    Function(String, Vec<String>, Block, TypeSpec),
    BuiltInFunction(BuiltInFunction)
}

#[derive(Clone)]
pub enum BuiltInFunction {
    WriteLn(fn(String) -> Object)
}

#[derive(Debug, Clone)]
pub enum Primitive {
    Integer(i32),
    Float(f32),
    String(String),
    Boolean(bool)
}

impl Object {

    pub fn add(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left + right))),
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left + right))),
            (&Object::Primitive(Primitive::String(ref left)), &Object::Primitive(Primitive::String(ref right)))
                => Ok(Object::Primitive(Primitive::String(format!("{}{}", left, right)))),
            _   => Err(String::from("Interpreter Addition Error"))
        };
    }

    pub fn subtract(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left - right))),
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left - right))),
            _   => Err(String::from("Interpreter Subtraction Error"))

        };
    }

    pub fn multiply(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left * right))),
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left * right))),
            _   => Err(String::from("Interpreter Multiplication Error"))

        };
    }

    pub fn integer_divide(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left / right))),
            _   => Err(String::from("Interpreter Integer Division Error"))

        };
    }

    pub fn float_divide(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left / right))),
            _   => Err(String::from("Interpreter Float Division Error"))

        };
    }

    pub fn unary_plus(&self) -> Result<Self, String> {
        return match self {
            &Object::Primitive(Primitive::Integer(i)) => Ok(Object::Primitive(Primitive::Integer(i))),
            &Object::Primitive(Primitive::Float(i))   => Ok(Object::Primitive(Primitive::Float(i))),
            _                                         => Err(String::from("Interpreter Unary Plus Error"))

        };
    }

    pub fn unary_minus(&self) -> Result<Self, String> {
        return match self {
            &Object::Primitive(Primitive::Integer(i)) => Ok(Object::Primitive(Primitive::Integer(-i))),
            &Object::Primitive(Primitive::Float(i))   => Ok(Object::Primitive(Primitive::Float(-i))),
            _                                         => Err(String::from("Interpreter Unary Minus Error"))
        };
    }

    pub fn and(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Boolean(left)), &Object::Primitive(Primitive::Boolean(right)))
                => Ok(Object::Primitive(Primitive::Boolean(left && right))),
            _   => Err(String::from("Interpreter Boolean And Error"))
        };
    }

    pub fn or(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Boolean(left)), &Object::Primitive(Primitive::Boolean(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left || right))),
            _   => Err(String::from("Interpreter Boolean Or Error"))
        };
    }

    pub fn negate(&self) -> Result<Self, String> {
        return match self {
            &Object::Primitive(Primitive::Boolean(b)) => Ok(Object::Primitive(Primitive::Boolean(!b))),
            _                                         => Err(String::from("Interpreter Negation Error"))
        };
    }

    pub fn less_than(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
                => Ok(Object::Primitive(Primitive::Boolean(left < right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
                => Ok(Object::Primitive(Primitive::Boolean(left < right))),
            _   => Err(String::from("Interpreter Less Than Error"))
        };
    }

    pub fn less_than_or_equal(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left <= right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left <= right))),
            _   => Err(String::from("Interpreter Less Than Or Equal Error"))
        };
    }

    pub fn greater_than(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left > right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left > right))),
            _   => Err(String::from("Interpreter Greater Than Error"))
        };
    }

    pub fn greater_than_or_equal(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left >= right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left >= right))),
            _   => Err(String::from("Interpreter Greater Than Or Equal Error"))
        };
    }

    pub fn equal(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left == right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left == right))),
            _   => Err(String::from("Interpreter Equal Error"))
        };
    }

    pub fn not_equal(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left != right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left != right))),
            _   => Err(String::from("Interpreter Not Equal Error"))
        };
    }

}

impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            &Object::Unit                                                  => write!(f, "<Unit>"),
            &Object::Primitive(ref primitive)                              => write!(f, "{:?}", primitive),
            &Object::Procedure(ref name, ref variables, _)                 => write!(f, "Procedure<{}, {:?}>", name, variables),
            &Object::Function(ref name, ref variables, _, ref return_type) => write!(f, "Function<{}, {:?} -> {:?}>", name, variables, return_type),
            &Object::BuiltInFunction(BuiltInFunction::WriteLn(_))          => write!(f, "BuiltInFunction<WriteLn, String>")
        };
    }
}
