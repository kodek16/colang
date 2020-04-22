use crate::program::Expression;
use crate::source::SourceOrigin;

pub struct ReadInstruction {
    pub target: Expression,

    /// If this flag is set, the next "clean" line is consumed from stdin completely.
    pub whole_line: bool,
    pub location: SourceOrigin,
}
