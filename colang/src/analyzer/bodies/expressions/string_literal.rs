use crate::context::CompilerContext;
use crate::source::SourceOrigin;
use crate::{ast, errors, escapes, program};

pub fn compile_string_literal_expr(
    expression: ast::StringLiteralExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let literal = escapes::unescape(&expression.value, expression.span);
    let literal = match literal {
        Ok(literal) => literal,
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    let literal = match String::from_utf8(literal) {
        Ok(literal) => literal,
        Err(_) => {
            let error = errors::literal_not_utf8(SourceOrigin::Plain(expression.span));
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    program::Expression::new(
        program::LiteralExpr {
            value: program::LiteralValue::String(literal),
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
