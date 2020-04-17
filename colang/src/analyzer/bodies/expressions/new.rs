use crate::analyzer::type_exprs;
use crate::{ast, program, CompilerContext};

pub fn compile_new_expr(
    expression: ast::NewExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let target_type =
        type_exprs::compile_type_expr_and_ensure_complete(&expression.target_type, context);
    if target_type.borrow().is_error() {
        return program::Expression::error(expression.span);
    }

    let result = program::NewExpr::new(target_type, context.program.types_mut(), expression.span);
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
