//! CO compiler backend interface definition.
//!
//! Backend is a part of the compiler that receives the compiled and type-checked program IR and
//! does something useful with it, for example, translating it into a target language, or executing
//! it directly.

use crate::program::Program;

/// Common interface for CO compiler backends.
pub trait Backend {
    /// Process the compiled CO program.
    ///
    /// Backends should report any user-caused errors to the user, and panic only if an internal
    /// error occurs. The returned `Result` indicates whether any user-caused errors occurred and
    /// were reported.
    fn run(&self, file_name: &str, source: &str, program: Program) -> Result<(), ()>;
}
