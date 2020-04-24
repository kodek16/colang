use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use std::env::Args;
use std::fs;

use colang::backends::Backend;
use colang::errors::CompilationError;
use colang_c_target::CBackend;
use colang_interpreter::InterpreterBackend;

pub struct Config {
    pub source_path: String,
    pub backend: Box<dyn Backend>,

    /// Instead of executing the program, dump an s-exp representation of it to stdout.
    pub debug: bool,

    /// A flag for integration tests: this allows to better capture output. There is
    /// no way to set it through command-line.
    pub plaintext_compilation_errors: bool,
}

impl Config {
    /// Parse command line arguments.
    pub fn new(args: Args) -> Result<Config, &'static str> {
        // TODO use a command line parser library.
        let mut args: Vec<String> = args.skip(1).collect();

        let debug = args.iter().any(|a| a == "--debug");
        let compile = args.iter().any(|a| a == "--compile");

        // Drop flags.
        args.retain(|f| !f.starts_with("--"));

        if args.len() >= 1 {
            let source_path = args[0].to_owned();
            Ok(Config {
                source_path,
                backend: if compile {
                    Box::new(CBackend)
                } else {
                    Box::new(InterpreterBackend)
                },
                debug,
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

    if config.debug {
        let result = colang::run_debug(&source_code);
        return if result.is_ok() {
            RunResult::Ok
        } else {
            RunResult::CompilerError
        };
    }

    let program = match colang::run(&source_code) {
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

    match config
        .backend
        .run(&config.source_path, &source_code, program)
    {
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
        let mut files = SimpleFiles::new();
        let user_program_id = files.add(file_name, source_code);
        let std_id = files.add("std.rs", colang::stdlib::STD_SOURCE);

        let writer = StandardStream::stderr(ColorChoice::Always);
        let config = codespan_reporting::term::Config::default();

        for error in errors {
            codespan_reporting::term::emit(
                &mut writer.lock(),
                &config,
                &files,
                &error.to_codespan(user_program_id, std_id),
            )
            .unwrap();
        }
    } else {
        for error in errors {
            println!("{:?}", error);
        }
    }
}
