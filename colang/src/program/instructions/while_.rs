use crate::program::Instruction;
use crate::program::{Expression, SourceOrigin};

pub struct WhileInstruction {
    pub cond: Box<Expression>,
    pub body: Box<Instruction>,
    pub location: SourceOrigin,
}
