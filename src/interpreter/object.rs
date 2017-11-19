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
    String(String)
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
            _   => Err(String::from("Can't add mismatching types"))
        };
    }

    pub fn subtract(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left - right))),
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left - right))),
            (&Object::Primitive(Primitive::String(_)), &Object::Primitive(Primitive::String(_)))
                => Err(String::from("Can't subtract string types")),
            _   => Err(String::from("Can't subtract mismatching types"))
        };
    }

    pub fn multiply(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left * right))),
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left * right))),
            (&Object::Primitive(Primitive::String(_)), &Object::Primitive(Primitive::String(_)))
                => Err(String::from("Can't multiple string types")),
            _   => Err(String::from("Can't multiply mismatching types"))
        };
    }

    pub fn integer_divide(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left / right))),
            (&Object::Primitive(Primitive::Float(_)), &Object::Primitive(Primitive::Float(_)))
                => Err(String::from("Can't integer divide float types")),
            (&Object::Primitive(Primitive::String(_)), &Object::Primitive(Primitive::String(_)))
                => Err(String::from("Can't integer divide string types")),
            _   => Err(String::from("Can't integer divide mismatching types"))
        };
    }

    pub fn float_divide(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left / right))),
            (&Object::Primitive(Primitive::Integer(_)), &Object::Primitive(Primitive::Integer(_)))
                => Err(String::from("Can't float divide integer types")),
            (&Object::Primitive(Primitive::String(_)), &Object::Primitive(Primitive::String(_)))
                => Err(String::from("Can't float divide string types")),
            _   => Err(String::from("Can't float divide mismatching types"))
        };
    }

    pub fn unary_plus(&self) -> Result<Self, String> {
        return match self {
            &Object::Primitive(Primitive::Integer(i)) => Ok(Object::Primitive(Primitive::Integer(i))),
            &Object::Primitive(Primitive::Float(i))   => Ok(Object::Primitive(Primitive::Float(i))),
            &Object::Primitive(Primitive::String(_))  => Err(String::from("Can't do unary plus with string type")),
            _                                         => Err(String::from("Can't do unary plus with procedures"))
        };
    }

    pub fn unary_minus(&self) -> Result<Self, String> {
        return match self {
            &Object::Primitive(Primitive::Integer(i)) => Ok(Object::Primitive(Primitive::Integer(-i))),
            &Object::Primitive(Primitive::Float(i))   => Ok(Object::Primitive(Primitive::Float(-i))),
            &Object::Primitive(Primitive::String(_))  => Err(String::from("Can't do unary minus with string type")),
            _                                         => Err(String::from("Can't do unary minus with procedures"))
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
