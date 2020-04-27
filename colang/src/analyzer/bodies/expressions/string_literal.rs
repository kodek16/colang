use crate::context::CompilerContext;
use crate::{ast, program};

pub fn compile_string_literal_expr(
    expression: ast::StringLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let result = program::LiteralExpr::string(
        &expression.value,
        context.program.types_mut(),
        expression.span,
    );
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
