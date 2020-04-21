use crate::program::Expression;
use crate::source::SourceOrigin;

pub struct ReturnInstruction {
    pub expression: Expression,
    pub location: SourceOrigin,
}