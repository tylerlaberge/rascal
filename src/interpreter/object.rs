use parser::ast::Block;

use std::fmt::Debug;
use std::fmt;

pub enum Object {
    Primitive(Primitive),
    Procedure(String, Vec<String>, Block)
}

#[derive(Debug)]
pub enum Primitive {
    Integer(i32),
    Float(f32)
}

impl Object {

    pub fn clone(&self) -> Self {
        return match self {
            &Object::Primitive(Primitive::Integer(i)) => Object::Primitive(Primitive::Integer(i)),
            &Object::Primitive(Primitive::Float(i))   => Object::Primitive(Primitive::Float(i)),
            &Object::Procedure(_, _, _)               => panic!("Cannot clone procedure")
        }
    }
    pub fn add(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left + right))),
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left + right))),
            _   => Err(String::from("Can't add incompatible types"))
        };
    }

    pub fn subtract(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left - right))),
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left - right))),
            _   => Err(String::from("Can't subtract incompatible types"))
        };
    }

    pub fn multiply(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left * right))),
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left * right))),
            _   => Err(String::from("Can't multiply incompatible types"))
        };
    }

    pub fn integer_divide(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Integer(ref left)), &Object::Primitive(Primitive::Integer(ref right)))
                => Ok(Object::Primitive(Primitive::Integer(left / right))),
            _   => Err(String::from("Can't integer divide non-integer types"))
        };
    }

    pub fn float_divide(&self, other: &Self) -> Result<Self, String> {
        return match (self, other) {
            (&Object::Primitive(Primitive::Float(ref left)), &Object::Primitive(Primitive::Float(ref right)))
                => Ok(Object::Primitive(Primitive::Float(left / right))),
            _   => Err(String::from("Can't float divide non-float types"))
        };
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            &Object::Primitive(ref primitive)              => write!(f, "{:?}", primitive),
            &Object::Procedure(ref name, ref variables, _) => write!(f, "Procedure<{}, {:?}>", name, variables)
        };
    }
}
