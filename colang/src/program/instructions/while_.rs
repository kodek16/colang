use crate::program::{Expression, Instruction};
use crate::source::SourceOrigin;

pub struct WhileInstruction {
    pub cond: Box<Expression>,
    pub body: Box<Instruction>,
    pub location: SourceOrigin,
}
