use std::io;
use std::error::Error;

use super::object::Object;
use super::object::Primitive;

pub fn writeln(text: String) -> Result<Object, String> {
    println!("{}", text);

    return Ok(Object::Unit);
}

pub fn readln() -> Result<Object, String> {
    let mut buffer = String::from("");
    return match io::stdin().read_line(&mut buffer) {
        Ok(_)  => Ok(Object::Primitive(Primitive::String(buffer))),
        Err(e) => Err(String::from(e.description()))
    };
}

pub fn int_to_string(value: i32) -> Result<Object, String> {
    return Ok(Object::Primitive(Primitive::String(value.to_string())));
}

pub fn real_to_string(value: f32) -> Result<Object, String> {
    return Ok(Object::Primitive(Primitive::String(value.to_string())));
}

pub fn string_to_int(text: String) -> Result<Object, String> {
    return match text.trim().parse::<i32>() {
        Ok(num) => Ok(Object::Primitive(Primitive::Integer(num))),
        Err(_)  => Err(String::from("Could not convert string to integer"))
    };
}

pub fn string_to_real(text: String) -> Result<Object, String> {
    return match text.trim().parse::<f32>() {
        Ok(num) => Ok(Object::Primitive(Primitive::Float(num))),
        Err(_)  => Err(String::from("Could not convert string to real"))
    };
}