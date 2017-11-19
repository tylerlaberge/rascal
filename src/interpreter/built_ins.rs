use super::object::Object;

pub fn writeln(text: String) -> Object {
    println!("{}", text);

    return Object::Unit;
}