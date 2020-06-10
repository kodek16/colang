use crate::analyzer::type_exprs;
use crate::context::CompilerContext;
use crate::source::SourceOrigin;
use crate::{ast, program};

pub fn compile_new_expr(
    expression: ast::NewExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let target_type =
        type_exprs::compile_type_expr_and_ensure_complete(&expression.target_type, context);
    if target_type.borrow().is_error() {
        return program::Expression::error(expression.span);
    }

    program::Expression::new(
        program::NewExpr {
            target_type: target_type.into(),
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
