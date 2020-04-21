use crate::errors::CompilationError;
use crate::program::instructions::Instruction;
use crate::program::{Expression, ValueCategory};
use crate::source::SourceOrigin;

pub struct AssignInstruction {
    pub target: Box<Expression>,
    pub value: Box<Expression>,
    pub location: SourceOrigin,
}

impl AssignInstruction {
    pub fn new(
        target: Expression,
        value: Expression,
        location: SourceOrigin,
    ) -> Result<Instruction, CompilationError> {
        if target.value_category() != ValueCategory::Lvalue {
            let error = CompilationError::assignment_target_not_lvalue(target.location());
            return Err(error);
        }

        let target_type = target.type_();
        let value_type = value.type_();

        if *target_type != *value_type {
            let error = CompilationError::assignment_type_mismatch(&target, &value, location);
            return Err(error);
        }

        Ok(Instruction::Assign(AssignInstruction {
            target: Box::new(target),
            value: Box::new(value),
            location,
        }))
    }
}
