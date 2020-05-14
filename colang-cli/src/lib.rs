use clap::{App, AppSettings, Arg, SubCommand};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use colang::backends::Backend;
use colang::errors::CompilationError;
use colang_c_target::CBackend;
use colang_interpreter::InterpreterBackend;
use std::fs;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

pub struct Config {
    pub source_path: String,

    /// The expected compiler behavior.
    pub target: Target,

    /// A flag for integration tests: this allows to better capture output. There is
    /// no way to set it through command-line.
    pub plaintext_compilation_errors: bool,
}

/// Expected result of running the compiler.
pub enum Target {
    /// Dump the internal debug information from the frontend.
    Debug,

    /// Expect successful compilation and pass the result to a backend.
    Run(Box<dyn Backend>),
}

impl Config {
    pub fn new() -> Config {
        let program = || {
            Arg::with_name("PROGRAM")
                .help("Sets the file with the CO program")
                .required(true)
                .index(1)
        };

        let matches = App::new("colang")
            .version(VERSION)
            .about("Compiler and interpreter for the CO language")
            .setting(AppSettings::SubcommandRequiredElseHelp)
            .subcommand(
                SubCommand::with_name("run")
                    .about("Executes the program in an interpreter")
                    .arg(program()),
            )
            .subcommand(
                SubCommand::with_name("compile")
                    .about("Compiles the program into C code")
                    .arg(program())
                    .arg(
                        Arg::with_name("output")
                            .short("o")
                            .long("--output")
                            .takes_value(true)
                            .help("Sets the path for the generated C file"),
                    ),
            )
            .subcommand(
                SubCommand::with_name("internal-dump-ir")
                    .about("Prints the internal program IR (internal command)")
                    .arg(program()),
            )
            .get_matches();

        let (subcommand, sub_matches) = matches.subcommand();
        let sub_matches = sub_matches.unwrap();

        Config {
            source_path: sub_matches.value_of("PROGRAM").unwrap().to_string(),
            target: match subcommand {
                "run" => Target::Run(Box::new(InterpreterBackend)),
                "compile" => Target::Run(Box::new(CBackend::new(sub_matches.value_of("output")))),
                "internal-dump-ir" => Target::Debug,
                _ => panic!("Unknown subcommand"),
            },
            plaintext_compilation_errors: false,
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

    match config.target {
        Target::Debug => {
            let result = colang::debug(&source_code);
            if result.is_ok() {
                RunResult::Ok
            } else {
                RunResult::CompilerError
            }
        }
        Target::Run(backend) => {
            let program = match colang::compile(&source_code) {
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

            match backend.run(&config.source_path, &source_code, program) {
                Ok(()) => RunResult::Ok,
                Err(()) => RunResult::RuntimeError,
            }
        }
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
