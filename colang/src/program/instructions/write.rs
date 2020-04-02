use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::instructions::Instruction;
use crate::program::{Expression, TypeRegistry};

#[derive(Debug)]
pub struct WriteInstruction {
    expression: Expression,
}

impl WriteInstruction {
    pub fn new(
        expression: Expression,
        types: &TypeRegistry,
        expr_location: InputSpan,
    ) -> Result<Instruction, CompilationError> {
        let expression_type = &expression.type_;
        if *expression_type != *types.int() {
            let error = CompilationError::write_value_not_int(
                &expression_type.borrow().name(),
                expr_location,
            );
            return Err(error);
        }

        Ok(Instruction::Write(WriteInstruction { expression }))
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}
