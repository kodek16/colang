use crate::program::Expression;
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
