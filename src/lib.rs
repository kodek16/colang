use lalrpop_util::lalrpop_mod;

mod ast;
mod program;
lalrpop_mod!(pub grammar);

use std::env::Args;
use std::error::Error;
use std::fs;

use program::Program;

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

    let program_ast = parse(&source_code)?;
    println!("Parsed! Program: {:#?}", program_ast);

    let program = compile(program_ast);
    println!("Compiled! Program: {:#?}", program);

    Ok(())
}

/// Parses the source code and returns an AST root.
fn parse(source_code: &str) -> Result<ast::Program, String> {
    grammar::ProgramParser::new()
        .parse(source_code)
        .map_err(|err| err.to_string())
}

/// Compiles a CO program.
fn compile(_program_ast: ast::Program) -> Program {
    Program::new()
}
