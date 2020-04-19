use crate::program::{Expression, SourceOrigin};

pub struct WriteInstruction {
    pub expression: Expression,
    pub location: SourceOrigin,
}
