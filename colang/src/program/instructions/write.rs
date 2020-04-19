use crate::errors::CompilationError;
use crate::program::instructions::Instruction;
use crate::program::{
    CallExpr, Expression, ExpressionKind, InternalFunctionTag, Program, SourceOrigin, TypeId,
};
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
            .location()
            .expect("Attempt to write a synthetic `int` expression");

        let stringified_expr = match expression_type.borrow().type_id {
            TypeId::String => expression,
            TypeId::Int => {
                let conversion = program.internal_function(InternalFunctionTag::IntToString);

                Expression::new(
                    ExpressionKind::Call(CallExpr {
                        function: Rc::clone(conversion),
                        arguments: vec![expression],
                        location: SourceOrigin::Stringified(expression_span),
                    }),
                    program.types_mut(),
                )
            }
            _ => {
                let error = CompilationError::write_value_is_not_stringable(
                    &expression_type.borrow().name,
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
