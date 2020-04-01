use std::env;
use std::process;

use colang::{run, Config, RunResult};

fn main() {
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        eprintln!("Error: {}.", err);
        process::exit(1);
    });

    if run(config) != RunResult::Ok {
        process::exit(1);
    }
}
