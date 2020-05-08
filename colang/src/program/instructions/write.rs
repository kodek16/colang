use crate::program::instructions::InstructionKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Instruction};
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

impl InstructionKind for WriteInstruction {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for WriteInstruction {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.expression)
    }
}
