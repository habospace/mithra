mod data;
mod parser;

use std::env;
use std::fs::File;
use std::io::{self, BufRead};

fn get_file_chars(file_path: String) -> Result<Vec<char>, std::io::Error> {
    let file = File::open(file_path)?;
    let mut chars: Vec<char> = Vec::new();
    for line in io::BufReader::new(file).lines() {
        for c in line?.chars() {
            chars.push(c);
        }
        chars.push('\n');
    }
    Ok(chars)
}

fn main() {
    let mut sys_args: Vec<String> = env::args().collect();
    match sys_args.pop() {
        Some(mithra_script_path) => match get_file_chars(mithra_script_path) {
            Ok(chars) => {
                let mut text: data::Text = data::Text::new(chars);
                let parser = parser::mithra_parsers::parse_inline_exprs(0);
                match parser(&mut text) {
                    Ok(exprs) => {
                        println!("Succesfully parsed '.mt' file: {:?}", exprs)
                    }
                    Err(err) => {
                        println!("Failed to parse '.mt' file: {:?}", err)
                    }
                }
            }
            Err(err) => {
                println!("Couldn't turn '.mt' file to chars: {:?}", err)
            }
        },
        None => {
            println!("Please specify a '.mt' script in command line args.")
        }
    }
}
