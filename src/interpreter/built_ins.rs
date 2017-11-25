use super::object::Object;
use super::object::Primitive;

pub fn writeln(text: String) -> Object {
    println!("{}", text);

    return Object::Unit;
}

pub fn int_to_string(value: i32) -> Object {
    return Object::Primitive(Primitive::String(value.to_string()));
}

pub fn real_to_string(value: f32) -> Object {
    return Object::Primitive(Primitive::String(value.to_string()));
}