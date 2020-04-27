use crate::analyzer::type_exprs;
use crate::source::SourceOrigin;
use crate::{ast, program, CompilerContext};

pub(crate) fn compile_new_expr(
    expression: ast::NewExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let target_type =
        type_exprs::compile_type_expr_and_ensure_complete(&expression.target_type, context);
    if target_type.borrow().is_error() {
        return program::Expression::error(expression.span);
    }

    // `void` cannot be referred to, so an error would have been already reported.
    assert!(target_type != *context.program.types().void());

    program::Expression::new(
        program::ExpressionKind::New(program::NewExpr {
            target_type,
            location: SourceOrigin::Plain(expression.span),
        }),
        context.program.types_mut(),
    )
}
