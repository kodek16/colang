use crate::{ast, program, CompilerContext};

pub(crate) fn compile_int_literal_expr(
    expression: ast::IntLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    program::LiteralExpr::int(
        expression.value,
        context.program.types_mut(),
        expression.span,
    )
}
