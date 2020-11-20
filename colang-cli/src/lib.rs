use clap::{App, AppSettings, Arg, ArgMatches, SubCommand};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
use colang::backends::Backend;
use colang::errors::CompilationError;
use colang::options::{AnalyzerOptions, ParserOptions};
use colang_c_target::CBackend;
use colang_interpreter::InterpreterBackend;
use std::fs;

const VERSION: &'static str = env!("CARGO_PKG_VERSION");

pub struct Config {
    pub source_path: String,

    pub experimental_parser: bool,

    /// The expected compiler behavior.
    pub target: Target,

    /// A flag that allows to compile a file without using the standard library,
    pub no_std: bool,

    /// A flag for integration tests: this allows to better capture output. There is
    /// no way to set it through command-line.
    pub plaintext_compilation_errors: bool,
}

/// Expected result of running the compiler.
pub enum Target {
    /// Display an interactive UI for exploring the AST.
    AstGui,

    /// Dump the internal debug information from the frontend.
    Debug,

    /// Expect successful compilation and pass the result to a backend.
    Run(Box<dyn Backend>),
}

impl Config {
    pub fn new() -> Config {
        let program = || {
            Arg::with_name("PROGRAM")
                .help("Path to the CO program")
                .required(true)
                .index(1)
        };

        let matches = App::new("colang")
            .version(VERSION)
            .about("Compiler and interpreter for the CO language")
            .setting(AppSettings::SubcommandRequiredElseHelp)
            .arg(
                Arg::with_name("use-experimental-parser")
                    .short("e")
                    .long("--experimental-parser")
                    .help("Use the new experimental parser")
                    .global(true),
            )
            .arg(
                Arg::with_name("no-std")
                    .long("--no-std")
                    .help("Do not use the standard library")
                    .global(true),
            )
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
                            .help("Path to use for the generated C file"),
                    ),
            )
            .subcommand(
                SubCommand::with_name("tools")
                    .about("Various language-level tools most useful for colang devs")
                    .subcommand(
                        SubCommand::with_name("ast")
                            .about("Displays the AST obtained from the parser")
                            .arg(program()),
                    )
                    .subcommand(
                        SubCommand::with_name("ir")
                            .about("Prints the compiled program IR")
                            .arg(program()),
                    ),
            )
            .get_matches();

        let config_from_leaf_matches = |matches: &ArgMatches, target| Config {
            source_path: matches.value_of("PROGRAM").unwrap().to_string(),
            experimental_parser: matches.is_present("use-experimental-parser"),
            no_std: matches.is_present("no-std"),
            plaintext_compilation_errors: false,
            target,
        };

        match matches.subcommand() {
            ("run", Some(matches)) => {
                config_from_leaf_matches(matches, Target::Run(Box::new(InterpreterBackend)))
            }
            ("compile", Some(matches)) => config_from_leaf_matches(
                matches,
                Target::Run(Box::new(CBackend::new(matches.value_of("output")))),
            ),
            ("tools", Some(matches)) => match matches.subcommand() {
                ("ast", Some(matches)) => config_from_leaf_matches(matches, Target::AstGui),
                ("ir", Some(matches)) => config_from_leaf_matches(matches, Target::Debug),
                _ => unreachable!(),
            },
            _ => unreachable!(),
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

    let parser_options = if config.experimental_parser {
        ParserOptions::Experimental
    } else {
        ParserOptions::Old
    };

    let analyzer_options = AnalyzerOptions {
        no_std: config.no_std,
    };

    match config.target {
        Target::AstGui => {
            let result = colang::parse(&source_code, parser_options);
            match result {
                Ok(_ast) => {
                    println!("Ok!");
                    RunResult::Ok
                }
                Err(errors) => {
                    report_compilation_errors(
                        &config.source_path,
                        &source_code,
                        &errors,
                        config.plaintext_compilation_errors,
                    );
                    RunResult::CompilerError
                }
            }
        }
        Target::Debug => {
            let result = colang::debug(&source_code, parser_options, analyzer_options);
            if result.is_ok() {
                RunResult::Ok
            } else {
                RunResult::CompilerError
            }
        }
        Target::Run(backend) => {
            let program = match colang::compile(&source_code, parser_options, analyzer_options) {
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
