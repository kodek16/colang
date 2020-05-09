use std::fs;
use std::process::Command;
use tempfile::tempdir;
use test_generator::test_resources;

#[test_resources("colang-cli/tests/samples/good/**/*.co")]
fn good_compile(path: &str) {
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

    let cpp_path = source_path.with_extension("cpp");
    let executable_path = source_path.with_extension(".run");

    let status = Command::new("g++")
        .arg(&cpp_path)
        .arg("-o")
        .arg(&executable_path)
        .status()
        .expect("g++ failed to execute");

    assert!(status.success(), "g++ compilation failed");

    let status = Command::new(&executable_path)
        .status()
        .expect("compiled program failed to execute");

    assert!(status.success(), "runtime error encountered");
}

// Because of how [test_resources] works with workspaces, the compilation working directory
// is different from the runtime working directory (first is workspace, second is crate).
// This is why we need to strip the first part from the test path.
fn strip_crate_name(path: &str) -> String {
    let parts: Vec<String> = path.split('/').skip(1).map(String::from).collect();
    parts.join("/")
}
