use crate::program::expressions::{Expression, ExpressionKind};
use crate::program::instructions::InstructionKind;
use crate::program::visitors::node::LocalCodeNode;
use crate::program::Instruction;
use crate::source::SourceOrigin;

/// An instruction that evaluates an expression and does not use its value.
///
/// This instruction creates one of the few "void contexts": `expression` can have type `void`.
pub struct EvalInstruction {
    /// Expression to be evaluated. Can be `void`.
    pub expression: Expression,
}

impl InstructionKind for EvalInstruction {
    fn location(&self) -> SourceOrigin {
        self.expression.location()
    }
}

impl<'a> LocalCodeNode<'a> for EvalInstruction {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.expression)
    }
}
