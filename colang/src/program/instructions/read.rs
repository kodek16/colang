use crate::errors::CompilationError;
use crate::program::instructions::Instruction;
use crate::program::{Expression, TypeRegistry, ValueCategory};

#[derive(Debug)]
pub struct ReadInstruction {
    target: Box<Expression>,
}

impl ReadInstruction {
    pub fn new(target: Expression, types: &TypeRegistry) -> Result<Instruction, CompilationError> {
        if target.value_category != ValueCategory::Lvalue {
            let error = CompilationError::read_target_not_lvalue(
                target.span.expect("Attempt to read generated rvalue."),
            );
            return Err(error);
        }

        let target_type = &target.type_;
        if *target_type != *types.int() {
            let error = CompilationError::read_target_not_int(
                target_type.borrow().name(),
                target
                    .span
                    .expect("Attempt to read generated non-int value."),
            );
            return Err(error);
        }

        Ok(Instruction::Read(ReadInstruction {
            target: Box::new(target),
        }))
    }

    pub fn target(&self) -> &Expression {
        &self.target
    }
}
