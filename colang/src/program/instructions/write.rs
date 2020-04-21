use crate::program::Expression;
use crate::source::SourceOrigin;

pub struct WriteInstruction {
    pub expression: Expression,
    pub location: SourceOrigin,
}
