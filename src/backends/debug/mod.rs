//! Debug backend that just dumps the program structure to stdout.

use super::Backend;
use crate::program::Program;

pub struct DebugBackend;

impl Backend for DebugBackend {
    fn run(&self, program: Program) -> Result<(), ()> {
        println!("Program is:\n{}", program);
        Ok(())
    }
}
