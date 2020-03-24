#[derive(Debug)]
pub struct Program;

use std::env::Args;
use std::error::Error;
use std::fs;
use std::io;

pub struct Config {
    source_path: String,
}

impl Config {
    /// Parse command line arguments.
    pub fn new(mut args: Args) -> Result<Config, &'static str> {
        args.next();

        if let Some(source_path) = args.next() {
            Ok(Config { source_path })
        } else {
            return Err("Missing path to source file");
        }
    }
}

/// Executes the program.
pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let source_code = fs::read_to_string(config.source_path)?;
    let program = compile(&source_code);

    println!("Compiled! Program: {:#?}", program);
    Ok(())
}

/// Compiles a CO program.
fn compile(_source_code: &str) -> Program {
    Program
}
