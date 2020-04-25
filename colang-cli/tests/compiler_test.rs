use test_generator::test_resources;

use colang_cli::{Config, RunResult};

#[test_resources("colang-cli/tests/samples/good/**/*.co")]
fn check_good_program(path: &str) {
    ()
}
