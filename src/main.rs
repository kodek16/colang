use std::env;
use std::process;

use colang::{run, Config};

fn main() {
    let config = Config::new(env::args()).unwrap_or_else(|err| {
        eprintln!("Error: {}.", err);
        process::exit(1);
    });

    if let Err(err) = run(config) {
        eprintln!("Error: {}.", err);
        process::exit(1);
    }
}
