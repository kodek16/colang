use lalrpop_util::lalrpop_mod;

mod ast;
pub mod backends;
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

pub struct Config {
    pub source_path: String,
    pub backend: Box<dyn Backend>,

    /// A flag for integration tests: this allows to better capture output. There is
    /// no way to set it through command-line.
    pub plaintext_compilation_errors: bool,
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
                plaintext_compilation_errors: false,
            })
        } else {
            return Err("Missing path to source file");
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum RunResult {
    Ok,
    CompilerError,
    RuntimeError,
}

/// Executes the program.
pub fn run(config: Config) -> RunResult {
    let source_code = fs::read_to_string(&config.source_path);
    let source_code = match source_code {
        Ok(source_code) => source_code,
        Err(error) => {
            eprintln!(
                "Error while reading the source file:\n{}",
                error.to_string()
            );
            return RunResult::CompilerError;
        }
    };

    let program = match frontend::run(&source_code) {
        Ok(program) => program,
        Err(errors) => {
            report_compilation_errors(
                &config.source_path,
                &source_code,
                &errors,
                config.plaintext_compilation_errors,
            );
            return RunResult::CompilerError;
        }
    };

    match config.backend.run(program) {
        Ok(()) => RunResult::Ok,
        Err(()) => RunResult::RuntimeError,
    }
}

fn report_compilation_errors(
    file_name: &str,
    source_code: &str,
    errors: &Vec<CompilationError>,
    plaintext_stdout: bool,
) {
    if !plaintext_stdout {
        let file = SimpleFile::new(file_name, source_code);
        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        for error in errors {
            codespan_reporting::term::emit(
                &mut writer.lock(),
                &config,
                &file,
                &error.to_codespan(),
            )
            .unwrap();
        }
    } else {
        for error in errors {
            println!("{:?}", error);
        }
    }
}
