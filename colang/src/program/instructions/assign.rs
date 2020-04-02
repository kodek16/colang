use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::instructions::Instruction;
use crate::program::{Expression, ValueCategory};

#[derive(Debug)]
pub struct AssignInstruction {
    target: Box<Expression>,
    value: Box<Expression>,
}

impl AssignInstruction {
    pub fn new(
        target: Expression,
        value: Expression,
        location: InputSpan,
    ) -> Result<Instruction, CompilationError> {
        if target.value_category != ValueCategory::Lvalue {
            let error = CompilationError::assignment_target_not_lvalue(
                target
                    .span
                    .expect("Generated rvalue expression used as assignment target"),
            );
            return Err(error);
        }

        let target_type = &target.type_;
        let value_type = &value.type_;

        if *target_type != *value_type {
            let error = CompilationError::assignment_type_mismatch(
                target_type.borrow().name(),
                value_type.borrow().name(),
                location,
            );
            return Err(error);
        }

        Ok(Instruction::Assign(AssignInstruction {
            target: Box::new(target),
            value: Box::new(value),
        }))
    }

    pub fn target(&self) -> &Expression {
        &self.target
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}
