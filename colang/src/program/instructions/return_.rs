use crate::program::instructions::Instruction;
use crate::program::Expression;

/// This instruction represents memorization of a value that would later
/// become the value of the surrounding context. This context might be a
/// function (and then this is the return value), but more commonly
/// this is the final value of an expression block.
pub struct ReturnInstruction {
    expression: Box<Expression>,
}

impl ReturnInstruction {
    pub fn new(expression: Expression) -> Instruction {
        Instruction::Return(ReturnInstruction {
            expression: Box::new(expression),
        })
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}
