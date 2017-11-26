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
    Write(fn(String) -> Result<Object, String>),
    WriteLn(fn(String) -> Result<Object, String>),
    ReadLn(fn() -> Result<Object, String>),
    IntToString(fn(i32) -> Result<Object, String>),
    RealToString(fn(f32) -> Result<Object, String>),
    StringToInt(fn(String) -> Result<Object, String>),
    StringToReal(fn(String) -> Result<Object, String>)
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
            _   => Err(String::from("Internal Interpreter Error: Attempted to add incompatible types"))
        };
    }

    pub fn subtract(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left - right))),
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left - right))),
            _   => Err(String::from("Interpreter Interpreter Error: Attempted to subtract incompatible types"))

        };
    }

    pub fn multiply(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left * right))),
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left * right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to multiply incompatible types"))

        };
    }

    pub fn integer_divide(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left / right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to integer divide incompatible types"))

        };
    }

    pub fn float_divide(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left / right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to float divide incompatible types"))

        };
    }

    pub fn unary_plus(&self) -> Result<Self, String> {
        return match self {
            &Object::Primitive(Primitive::Integer(i)) => Ok(Object::Primitive(Primitive::Integer(i))),
            &Object::Primitive(Primitive::Float(i))   => Ok(Object::Primitive(Primitive::Float(i))),
            _                                         => Err(String::from("Internal Interpreter Error: Attempted to unary add incompatible type"))

        };
    }

    pub fn unary_minus(&self) -> Result<Self, String> {
        return match self {
            &Object::Primitive(Primitive::Integer(i)) => Ok(Object::Primitive(Primitive::Integer(-i))),
            &Object::Primitive(Primitive::Float(i))   => Ok(Object::Primitive(Primitive::Float(-i))),
            _                                         => Err(String::from("Internal Interpreter Error: Attempted to unary subtract incompatible type"))
        };
    }

    pub fn and(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Boolean(left)), &Object::Primitive(Primitive::Boolean(right)))
                => Ok(Object::Primitive(Primitive::Boolean(left && right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to boolean 'and' incompatible types"))
        };
    }

    pub fn or(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Boolean(left)), &Object::Primitive(Primitive::Boolean(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left || right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to boolean 'or' incompatible types"))
        };
    }

    pub fn negate(&self) -> Result<Self, String> {
        return match self {
            &Object::Primitive(Primitive::Boolean(b)) => Ok(Object::Primitive(Primitive::Boolean(!b))),
            _                                         => Err(String::from("Internal Interpreter Error: Attempted to boolean negate incompatible type"))
        };
    }

    pub fn less_than(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
                => Ok(Object::Primitive(Primitive::Boolean(left < right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
                => Ok(Object::Primitive(Primitive::Boolean(left < right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to compare 'less than' with incompatible types"))
        };
    }

    pub fn less_than_or_equal(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left <= right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left <= right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to compare 'less than or equal' with incompatible types"))
        };
    }

    pub fn greater_than(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left > right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left > right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to compare 'greater than' with incompatible types"))
        };
    }

    pub fn greater_than_or_equal(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left >= right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left >= right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to compare 'greater than or equal' with incompatible types"))
        };
    }

    pub fn equal(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left == right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left == right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to compare 'equal' with incompatible types"))
        };
    }

    pub fn not_equal(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(left)), &Object::Primitive(Primitive::Integer(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left != right))),
            (&Object::Primitive(Primitive::Float(left)), &Object::Primitive(Primitive::Float(right)))
            => Ok(Object::Primitive(Primitive::Boolean(left != right))),
            _   => Err(String::from("Internal Interpreter Error: Attempted to compare 'not equal' with incompatible types"))
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
            &Object::BuiltInFunction(BuiltInFunction::Write(_))            => write!(f, "BuiltInFunction<Write, (String) -> ()>"),
            &Object::BuiltInFunction(BuiltInFunction::WriteLn(_))          => write!(f, "BuiltInFunction<WriteLn, (String) -> ()>"),
            &Object::BuiltInFunction(BuiltInFunction::ReadLn(_))           => write!(f, "BuiltInFunction<ReadLn, () -> String>"),
            &Object::BuiltInFunction(BuiltInFunction::IntToString(_))      => write!(f, "BuiltInFunction<IntToString, (Integer) -> String>"),
            &Object::BuiltInFunction(BuiltInFunction::RealToString(_))     => write!(f, "BuiltInFunction<RealToString, (Real) -> String>"),
            &Object::BuiltInFunction(BuiltInFunction::StringToInt(_))      => write!(f, "BuiltInFunction<StringToInt, (String) -> Integer>"),
            &Object::BuiltInFunction(BuiltInFunction::StringToReal(_))     => write!(f, "BuiltInFunction<StringToReal, (String) -> Real>")
        };
    }
}
