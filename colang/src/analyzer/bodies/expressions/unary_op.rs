use super::compile_expression;
use crate::context::CompilerContext;
use crate::errors::CompilationError;
use crate::source::SourceOrigin;
use crate::{ast, program};

pub fn compile_unary_op_expression(
    expression: ast::UnaryOperatorExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let operand = compile_expression(*expression.operand, None, context);
    if operand.is_error() {
        return program::Expression::error(expression.span);
    }

    match expression.operator {
        ast::UnaryOperator::LogicalNot => {
            if !operand.type_().borrow().is_bool() {
                let error = CompilationError::logical_operator_operand_wrong_type(
                    &expression.operator.to_string(),
                    &operand,
                );
                context.errors.push(error);
            }

            program::Expression::new(
                program::BooleanOpExpr {
                    op: program::BooleanOp::Not(Box::new(operand)),
                    location: SourceOrigin::Plain(expression.span),
                },
                context.program.types_mut(),
            )
        }
    }
}
