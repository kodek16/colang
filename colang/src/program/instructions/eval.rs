use crate::program::instructions::Instruction;
use crate::program::Expression;

pub struct EvalInstruction {
    expression: Box<Expression>,
}

impl EvalInstruction {
    pub fn new(expression: Expression) -> Instruction {
        Instruction::Eval(EvalInstruction {
            expression: Box::new(expression),
        })
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}
