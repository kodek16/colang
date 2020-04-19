use crate::analyzer::type_exprs;
use crate::errors::CompilationError;
use crate::program::SourceOrigin;
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

    if target_type == *context.program.types().void() {
        let error =
            CompilationError::new_expression_void_type(SourceOrigin::Plain(expression.span));
        context.errors.push(error);
        return program::Expression::error(expression.span);
    }

    program::Expression::new(
        program::ExpressionKind::New(program::NewExpr {
            target_type,
            location: SourceOrigin::Plain(expression.span),
        }),
        context.program.types_mut(),
    )
}
