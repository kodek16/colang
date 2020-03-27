use lalrpop_util::lalrpop_mod;

mod ast;
mod backends;
mod errors;
mod frontend;
mod program;
mod scope;
mod typing;
lalrpop_mod!(pub grammar);

use codespan_reporting::files::SimpleFile;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::env::Args;
use std::fs;

use backends::debug::DebugBackend;
use backends::interpreter::InterpreterBackend;
use backends::Backend;
use errors::CompilationError;
use std::error::Error;

pub struct Config {
    source_path: String,
    backend: Box<dyn Backend>,
}

impl Config {
    /// Parse command line arguments.
    pub fn new(args: Args) -> Result<Config, &'static str> {
        // TODO use a command line parser library.
        let mut args: Vec<String> = args.skip(1).collect();
        let backend: Box<dyn Backend> = if args.iter().any(|a| a == "--debug") {
            Box::new(DebugBackend)
        } else {
            Box::new(InterpreterBackend)
        };

        // Drop flags.
        args.retain(|f| !f.starts_with("--"));

        if args.len() >= 1 {
            let source_path = args[0].to_owned();
            Ok(Config {
                source_path,
                backend,
            })
        } else {
            return Err("Missing path to source file");
        }
    }
}

/// Executes the program.
pub fn run(config: Config) -> Result<(), Box<dyn Error>> {
    let source_code = fs::read_to_string(&config.source_path)?;

    let program = match frontend::run(&source_code) {
        Ok(program) => program,
        Err(errors) => {
            report_compilation_errors(&config.source_path, &source_code, &errors)?;
            return Err("compilation did not succeed".to_string().into());
        }
    };

    config.backend.run(program)?;
    Ok(())
}

fn report_compilation_errors(
    file_name: &str,
    source_code: &str,
    errors: &Vec<CompilationError>,
) -> std::io::Result<()> {
    let file = SimpleFile::new(file_name, source_code);
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    for error in errors {
        codespan_reporting::term::emit(&mut writer.lock(), &config, &file, &error.to_codespan())?;
    }

    Ok(())
}
