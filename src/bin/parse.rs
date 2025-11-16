use std::{fs::File, io::Read, path::Path};

use ore_c_rust::create_parser;

pub fn main() -> Result<(), std::io::Error> {
    let file_path_str = "sample.c";
    let path = Path::new(file_path_str);
    let mut file = File::open(path)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    let mut parser = create_parser(&input);
    let program = parser.parse_program();
    println!("{:#?}", program);
    Ok(())
}
