use crate::program::Expression;
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
