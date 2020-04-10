use crate::errors::CompilationError;
use crate::program::instructions::Instruction;
use crate::program::{checks, Expression, TypeRegistry};

pub struct WhileInstruction {
    cond: Box<Expression>,
    body: Box<Instruction>,
}

impl WhileInstruction {
    pub fn new(
        cond: Expression,
        body: Instruction,
        types: &TypeRegistry,
    ) -> Result<Instruction, CompilationError> {
        checks::check_condition_is_bool(&cond, types)?;

        Ok(Instruction::While(WhileInstruction {
            cond: Box::new(cond),
            body: Box::new(body),
        }))
    }

    pub fn cond(&self) -> &Expression {
        &self.cond
    }

    pub fn body(&self) -> &Instruction {
        &self.body
    }
}
