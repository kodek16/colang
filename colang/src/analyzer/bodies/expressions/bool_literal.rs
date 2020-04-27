use crate::context::CompilerContext;
use crate::{ast, program};

pub fn compile_bool_literal_expr(
    expression: ast::BoolLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    program::LiteralExpr::bool(
        expression.value,
        context.program.types_mut(),
        expression.span,
    )
}
