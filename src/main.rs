extern crate rascal;

use std::env;
use std::io::Read;
use std::fs::File;

fn main() {
    let filename = get_file_name().unwrap();
    let source = read_source_file(filename.as_str()).unwrap();

    rascal::interpret(source);
}

fn get_file_name() -> Result<String, String> {
    let mut args = env::args();
    args.next();

    return args.next().ok_or(String::from("Missing filename"));
}

fn read_source_file(filename: &str) -> Result<String, String> {
    let mut file = File::open(filename).or(Err("Couldn't open file"))?;
    let mut contents = String::new();
    file.read_to_string(&mut contents).or(Err("Could not read file"))?;

    return Ok(contents);
}