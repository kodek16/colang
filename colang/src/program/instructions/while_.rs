use crate::program::Expression;
use crate::program::Instruction;

pub struct WhileInstruction {
    pub cond: Box<Expression>,
    pub body: Box<Instruction>,
}
