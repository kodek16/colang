use std::process;

use colang_cli::{run, Config, RunResult};

fn main() {
    if run(Config::new()) != RunResult::Ok {
        process::exit(1);
    }
}
