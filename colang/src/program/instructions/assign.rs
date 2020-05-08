use crate::program::instructions::InstructionKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Instruction};
use crate::source::SourceOrigin;

/// An instruction that updates an lvalue setting it to a new value.
pub struct AssignInstruction {
    /// The lvalue to be updated.
    ///
    /// Must be an lvalue.
    pub target: Expression,

    /// The new value to be set.
    ///
    /// Must have the same type as `target`.
    pub value: Expression,

    /// The location of source code that produced this instruction.
    pub location: SourceOrigin,
}

impl InstructionKind for AssignInstruction {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for AssignInstruction {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::vec::IntoIter<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        vec![&mut self.target, &mut self.value].into_iter()
    }
}
