extern crate rascal;

use std::env;
use std::io::Read;
use std::fs::File;

fn main() {
    let filename = match get_file_name() {
        Ok(filename) => filename,
        Err(e)       => panic!(e)
    };
    let source = match read_source_file(filename.as_str()) {
        Ok(source) => source,
        Err(e)     => panic!(e)
    };

    rascal::interpret(source);
}

fn get_file_name() -> Result<String, String> {
    let mut args = env::args();
    args.next();

    return match args.next() {
        Some(filename) => Ok(filename),
        None           => Err(String::from("Missing filename"))
    };
}

fn read_source_file(filename: &str) -> Result<String, String> {
    let mut file = match File::open(filename) {
        Ok(file) => Ok(file),
        Err(_)   => Err("Couldn't open file")
    }?;
    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_)  => Ok(()),
        Err(_) => Err("Could not read file")
    }?;

    return Ok(contents);
}