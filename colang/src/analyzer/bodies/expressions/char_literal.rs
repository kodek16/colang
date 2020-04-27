use crate::{ast, program, CompilerContext};

pub(crate) fn compile_char_literal_expr(
    expression: ast::CharLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let result = program::LiteralExpr::char(
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
