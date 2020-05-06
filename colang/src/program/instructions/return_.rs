use crate::program::instructions::InstructionKind;
use crate::program::Expression;
use crate::source::SourceOrigin;

/// An instruction that immediately stops the execution of the current function.
///
/// The function return value is set to `expression`.
pub struct ReturnInstruction {
    /// The return value for the current function.
    ///
    /// Must have the same type as the function return type.
    pub expression: Expression,

    /// The location of the source code that produced this instruction.
    pub location: SourceOrigin,
}

impl InstructionKind for ReturnInstruction {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}
