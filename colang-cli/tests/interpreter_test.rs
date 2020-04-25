use test_generator::test_resources;

use colang_cli::{Config, RunResult};
use colang_interpreter::InterpreterBackend;

#[test_resources("colang-cli/tests/samples/good/**/*.co")]
fn check_good_program(path: &str) {
    let config = Config {
        source_path: strip_crate_name(path),
        backend: Box::new(InterpreterBackend),
        debug: false,
        plaintext_compilation_errors: false,
    };

    let result = colang_cli::run(config);
    assert_eq!(result, RunResult::Ok)
}

#[test_resources("colang-cli/tests/samples/compile_error/**/*.co")]
fn check_compile_error_program(path: &str) {
    let config = Config {
        source_path: strip_crate_name(path),
        backend: Box::new(InterpreterBackend),
        debug: false,
        plaintext_compilation_errors: true,
    };

    let result = colang_cli::run(config);
    assert_eq!(result, RunResult::CompilerError)
}

#[test_resources("colang-cli/tests/samples/runtime_error/**/*.co")]
fn check_runtime_error_program(path: &str) {
    let config = Config {
        source_path: strip_crate_name(path),
        backend: Box::new(InterpreterBackend),
        debug: false,
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
