use crate::context::CompilerContext;
use crate::source::SourceOrigin;
use crate::{ast, program};

pub fn compile_bool_literal_expr(
    expression: ast::BoolLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    program::Expression::new(
        program::LiteralExpr {
            value: program::LiteralValue::Bool(expression.value),
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
