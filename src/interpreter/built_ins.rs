use std::io;
use std::io::Write;
use std::error::Error;
use super::object::{Object, Primitive};

pub fn write(text: String) -> Result<Object, String> {
    print!("{}", text);
    io::stdout().flush().expect("Could not flush stdout");

    return Ok(Object::Unit);
}

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

#[cfg(test)]
mod tests {
    use super::Object;
    use super::Primitive;

    #[test]
    fn int_to_string() {
        assert_eq!(super::int_to_string(5), Ok(Object::Primitive(Primitive::String(String::from("5")))));
    }

    #[test]
    fn real_to_string() {
        assert_eq!(super::real_to_string(5.5), Ok(Object::Primitive(Primitive::String(String::from("5.5")))));
    }

    #[test]
    fn string_to_int() {
        assert_eq!(super::string_to_int(String::from("5")), Ok(Object::Primitive(Primitive::Integer(5))));
    }

    #[test]
    fn string_to_real() {
        assert_eq!(super::string_to_real(String::from("5.5")), Ok(Object::Primitive(Primitive::Float(5.5))));
    }
}