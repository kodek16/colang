use crate::errors::CompilationError;
use crate::program::instructions::Instruction;
use crate::program::{CallExpr, Expression, InternalFunctionTag, Program, TypeId};
use std::rc::Rc;

pub struct WriteInstruction {
    pub expression: Expression,
}

impl WriteInstruction {
    pub fn new(
        expression: Expression,
        program: &mut Program,
    ) -> Result<Instruction, CompilationError> {
        let expression_type = Rc::clone(expression.type_());
        let expression_span = expression
            .span
            .expect("Attempt to write a synthetic `int` expression");

        let stringified_expr = match expression_type.borrow().type_id() {
            TypeId::String => expression,
            TypeId::Int => {
                let conversion = program.internal_function(InternalFunctionTag::IntToString);
                CallExpr::new(
                    Rc::clone(&conversion),
                    vec![expression],
                    program.types_mut(),
                    expression_span,
                )
                .expect("Couldn't construct a call to <int as string>")
            }
            _ => {
                let error = CompilationError::write_value_is_not_stringable(
                    &expression_type.borrow().name(),
                    expression_span,
                );
                return Err(error);
            }
        };

        Ok(Instruction::Write(WriteInstruction {
            expression: stringified_expr,
        }))
    }
}
