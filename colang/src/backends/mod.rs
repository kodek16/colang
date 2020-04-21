//! This module contains various backends for CO compiler.
//! Backend is the part of the compiler where the code is sent to
//! after it's compiled and type-checked.

use crate::program::Program;

/// Common backend interface.
pub trait Backend {
    /// Process the compiled CO program. Backends should report any user-caused errors to
    /// the user, and panic if any internal error occurs. The returned `Result` indicates
    /// whether any user-caused errors occurred and were reported.
    fn run(&self, file_name: &str, source: &str, program: Program) -> Result<(), ()>;
}
