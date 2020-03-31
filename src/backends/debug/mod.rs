//! Debug backend that just dumps the program structure to stdout.

use super::Backend;
use crate::program::Program;
use std::error::Error;

pub struct DebugBackend;

impl Backend for DebugBackend {
    fn run(&self, program: Program) -> Result<(), Box<dyn Error>> {
        println!("Program is:\n{}", program);
        Ok(())
    }
}
