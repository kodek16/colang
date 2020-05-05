use crate::program::Expression;
use crate::source::SourceOrigin;

/// An instruction that writes a string to stdout.
pub struct WriteInstruction {
    /// The string to be written.
    ///
    /// Must have type `string`.
    pub expression: Expression,

    /// The location in the source code that produced this instruction.
    pub location: SourceOrigin,
}
