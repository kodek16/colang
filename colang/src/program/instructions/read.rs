use crate::program::instructions::InstructionKind;
use crate::program::visitors::node::LocalCodeNode;
use crate::program::{Expression, Instruction};
use crate::source::SourceOrigin;

/// An instruction that reads from stdin and stores the result in an lvalue.
pub struct ReadInstruction {
    /// Where to store the read data.
    ///
    /// Must be lvalue of either type `string` or `int`. If the type is `int`, `whole_line` must
    /// be false.
    pub target: Expression,

    /// If this flag is set, the next "clean" line is consumed from stdin completely.
    pub whole_line: bool,

    /// The location of the source code that produced this instruction.
    pub location: SourceOrigin,
}

impl InstructionKind for ReadInstruction {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for ReadInstruction {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.target)
    }
}
