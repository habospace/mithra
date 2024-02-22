mod data;
mod evaluator;
mod parser;

use data::MithraVal;
use data::Text;
use evaluator::evaluator::Program;
use evaluator::evaluator::Scope;
use parser::mithra_parsers;
use std::fs::File;
use std::io::{self, BufRead};
use std::process;

use rustyline::DefaultEditor;

fn interactive_mithra(program: &mut Program) {
    fn display_mithraval(val: MithraVal) {
        match val {
            MithraVal::Null => println!("{}", val),
            MithraVal::Int(_) => println!("{}", val),
            MithraVal::Float(_) => println!("{}", val),
            MithraVal::String(_) => println!("{}", val),
            MithraVal::Char(_) => println!("{}", val),
            MithraVal::Bool(_) => println!("{}", val),
            MithraVal::Variable(_, _) => println!("{}", val),
            MithraVal::List(_, _) => println!("{}", val),
            MithraVal::Dict(_, _) => println!("{}", val),
            MithraVal::Function(_, _) => println!("{}", val),
            _ => {}
        }
    }

    let mut rl = DefaultEditor::new().expect("Couldn't create editor...");
    if rl.load_history("history.txt").is_err() {
        //println!("No previous history.");
    }
    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                if line == format!("exit") {
                    break;
                }
                let _ = rl.add_history_entry(line.as_str());
                match mithra_parsers::parse_inline_exprs(0)(&mut Text::new(line.chars().collect()))
                {
                    Ok(exprs) => {
                        let result = program.run(&exprs);
                        match result {
                            Ok(val) => display_mithraval(val),
                            Err(runtime_err) => println! {"{}", runtime_err},
                        }
                    }
                    Err(parse_err) => {
                        println!("{}", parse_err)
                    }
                }
            }
            Err(_no_updates_is_fine) => {}
        }
    }
}

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
    let arg = format!("src/mithra_defaults/defaults.mth");
    let file_chars = get_file_chars(arg).expect("Couldn't turn '.mth' file to chars");
    match mithra_parsers::parse_inline_exprs(0)(&mut Text::new(file_chars)) {
        Ok(exprs) => {
            let mut program = Program::new(Scope::Global);
            match program.run(&exprs) {
                Ok(_) => {
                    println!("\nMithra 1.0.0 (main, Jan 01 2024, 12:00:00)");
                    println!("Running on: rustc 1.76.0 (07dca489a 2024-02-04)");
                    println!("Type 'exit' to quit the interactive session.");
                    interactive_mithra(&mut program);
                    //println!("\nProgram memory: {:?}", program.memory);
                    process::exit(0);
                }
                Err(err) => {
                    println!("Failed to evaluate 'defaults.mth': {:?}", err);
                    process::exit(1);
                }
            }
        }
        Err(err) => {
            println!("Failed to parse 'defaults.mth' file: {:?}", err);
            process::exit(1);
        }
    }
}
