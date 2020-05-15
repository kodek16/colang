//! Runner for CO code samples under features/.
//!
//! Feature tests are meant to be kept simple and short, and serve as an illustration for the
//! feature in question.

use std::fs;
use std::process::{Command, Stdio};
use tempfile::tempdir;
use test_generator::test_resources;

use colang_cli::{Config, RunResult, Target};
use colang_interpreter::InterpreterBackend;

#[test_resources("colang-cli/tests/samples/**/good/*.co")]
fn good_interpret(path: &str) {
    let config = Config {
        source_path: strip_crate_name(path),
        target: Target::Run(Box::new(InterpreterBackend)),
        plaintext_compilation_errors: false,
    };

    let result = colang_cli::run(config);
    assert_eq!(result, RunResult::Ok)
}

#[test_resources("colang-cli/tests/samples/**/good/*.co")]
fn good_compile_gpp(path: &str) {
    good_compile(path, "g++")
}

#[test_resources("colang-cli/tests/samples/**/good/*.co")]
fn good_compile_gcc(path: &str) {
    good_compile(path, "gcc")
}

fn good_compile(path: &str, compiler: &str) {
    let dir = tempdir().expect("Could not create a temporary directory");

    let source_path = dir.path().join("program.co");
    fs::copy(strip_crate_name(path), &source_path)
        .expect("Could not copy source file to temporary directory");

    let status = Command::new(env!("CARGO_BIN_EXE_colang-cli"))
        .arg("compile")
        .arg(&source_path)
        .status()
        .expect("colang-cli failed to execute");

    assert!(status.success(), "CO compilation failed");

    let c_path = source_path.with_extension("c");
    let executable_path = source_path.with_extension(".run");

    let status = Command::new(compiler)
        .arg(&c_path)
        .arg("-o")
        .arg(&executable_path)
        .status()
        .expect(&format!("{} failed to execute", compiler));

    assert!(status.success(), "{} compilation failed", compiler);

    let status = Command::new(&executable_path)
        .stdout(Stdio::null())
        .status()
        .expect("compiled program failed to execute");

    assert!(status.success(), "runtime error encountered");
}

#[test_resources("colang-cli/tests/samples/**/compile_error/*.co")]
fn compile_error(path: &str) {
    let config = Config {
        source_path: strip_crate_name(path),
        target: Target::Run(Box::new(InterpreterBackend)),
        plaintext_compilation_errors: true,
    };

    let result = colang_cli::run(config);
    assert_eq!(result, RunResult::CompilerError)
}

#[test_resources("colang-cli/tests/samples/**/runtime_error/*.co")]
fn runtime_error_interpret(path: &str) {
    let config = Config {
        source_path: strip_crate_name(path),
        target: Target::Run(Box::new(InterpreterBackend)),
        plaintext_compilation_errors: false,
    };

    let result = colang_cli::run(config);
    assert_eq!(result, RunResult::RuntimeError)
}

// Because of how [test_resources] works with workspaces, the compilation working directory
// is different from the runtime working directory (first is workspace, second is crate).
// This is why we need to strip the first part from the test path.
fn strip_crate_name(path: &str) -> String {
    let parts: Vec<String> = path.split('/').skip(1).map(String::from).collect();
    parts.join("/")
}
