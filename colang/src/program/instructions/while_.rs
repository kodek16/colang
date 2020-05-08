use crate::program::instructions::InstructionKind;
use crate::program::visitors::LocalCodeNode;
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

impl<'a> LocalCodeNode<'a> for WhileInstruction {
    type InstrIter = std::iter::Once<&'a mut Instruction>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::once(&mut self.body)
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.cond)
    }
}
