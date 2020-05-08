use crate::program::instructions::InstructionKind;
use crate::program::visitors::node::LocalCodeNode;
use crate::program::{Expression, Instruction};
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

impl<'a> LocalCodeNode<'a> for ReturnInstruction {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.expression)
    }
}
