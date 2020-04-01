use test_generator::test_resources;

use colang::backends::debug::DebugBackend;
use colang::{backends::interpreter::InterpreterBackend, Config, RunResult};

#[test_resources("tests/samples/good/**/*.co")]
fn check_good_program(path: &str) {
    let config = Config {
        source_path: { path }.to_string(),
        backend: Box::new(InterpreterBackend),
        plaintext_compilation_errors: false,
    };

    let result = colang::run(config);
    assert_eq!(result, RunResult::Ok)
}

#[test_resources("tests/samples/compile_error/**/*.co")]
fn check_compile_error_program(path: &str) {
    let config = Config {
        source_path: { path }.to_string(),
        backend: Box::new(DebugBackend),
        plaintext_compilation_errors: true,
    };

    let result = colang::run(config);
    assert_eq!(result, RunResult::CompilerError)
}

#[test_resources("tests/samples/runtime_error/**/*.co")]
fn check_runtime_error_program(path: &str) {
    let config = Config {
        source_path: { path }.to_string(),
        backend: Box::new(InterpreterBackend),
        plaintext_compilation_errors: false,
    };

    let result = colang::run(config);
    assert_eq!(result, RunResult::RuntimeError)
}
