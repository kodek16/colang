//! This module contains various backends for CO compiler.
//! Backend is the part of the compiler where the code is sent to
//! after it's compiled and type-checked.

use crate::program::Program;
use std::error::Error;

pub mod debug;
pub mod interpreter;

/// Common backend interface.
pub trait Backend {
    /// Process the compiled CO program.
    fn run(&self, program: Program) -> Result<(), Box<dyn Error>>;
}
