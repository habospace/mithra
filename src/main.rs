mod data;
mod parser;

use data::Text;
use parser::mithra_parsers;
use std::env;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

fn main() {
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
    let mut sys_args: Vec<String> = env::args().collect();
    let arg = sys_args
        .pop()
        .expect("Please specify a '.mth' script in command line args");
    let file_chars = get_file_chars(arg).expect("Couldn't turn '.mth' file to chars");
    match mithra_parsers::parse_inline_exprs(0)(&mut Text::new(file_chars)) {
        Ok(exprs) => {
            println!("Succesfully parsed '.mt' file: {:?}", exprs);
            process::exit(0);
        }
        Err(err) => {
            println!("Failed to parse '.mth' file: {:?}", err);
            process::exit(1);
        }
    }
}
