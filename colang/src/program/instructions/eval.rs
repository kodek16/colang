use crate::program::expressions::{Expression, ExpressionKind};
use crate::program::instructions::InstructionKind;
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
