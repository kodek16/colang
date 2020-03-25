//! Interpreter backend for CO can run the code right after it is compiled.

use super::Backend;
use crate::program::Program;
use std::error::Error;

pub struct InterpreterBackend;

impl Backend for InterpreterBackend {
    fn run(&self, program: Program) -> Result<(), Box<dyn Error>> {
        println!("Program is: {:#?}", program);
        Ok(())
    }
}
