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
