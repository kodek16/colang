use crate::program::instructions::InstructionKind;
use crate::program::{Expression, Instruction};
use crate::source::SourceOrigin;

/// An instruction that runs an expression and a statement in a loop until expression becomes false.
pub struct WhileInstruction {
    /// The loop breaking condition.
    ///
    /// Must be of type `bool`.
    pub cond: Expression,

    /// The body of the loop.
    pub body: Box<Instruction>,

    /// The location of the source code that produced this instruction.
    pub location: SourceOrigin,
}

impl InstructionKind for WhileInstruction {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}
