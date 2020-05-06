use crate::context::CompilerContext;
use crate::source::SourceOrigin;
use crate::{ast, program};

pub fn compile_int_literal_expr(
    expression: ast::IntLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    program::Expression::new(
        program::LiteralExpr {
            value: program::LiteralValue::Int(expression.value),
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
