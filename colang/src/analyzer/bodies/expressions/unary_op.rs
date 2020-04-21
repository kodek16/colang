use super::compile_expression;
use crate::errors::CompilationError;
use crate::source::SourceOrigin;
use crate::{ast, program, CompilerContext};

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
            if operand.type_() != context.program.types().bool() {
                let error = CompilationError::logical_operator_operand_wrong_type(
                    &expression.operator.to_string(),
                    &operand,
                );
                context.errors.push(error);
            }

            let kind = program::ExpressionKind::BooleanOp(program::BooleanOpExpr {
                op: program::BooleanOp::Not(Box::new(operand)),
                location: SourceOrigin::Plain(expression.span),
            });
            program::Expression::new(kind, context.program.types_mut())
        }
    }
}
