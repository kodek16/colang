use crate::context::CompilerContext;
use crate::errors::CompilationError;
use crate::source::SourceOrigin;
use crate::{ast, escapes, program};

pub fn compile_char_literal_expr(
    expression: ast::CharLiteralExpr,
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

    if literal.len() != 1 {
        let error = CompilationError::char_literal_bad_length(
            literal.len(),
            SourceOrigin::Plain(expression.span),
        );
        context.errors.push(error);
        return program::Expression::error(expression.span);
    };

    let character = literal[0];

    program::Expression::new(
        program::LiteralExpr {
            value: program::LiteralValue::Char(character),
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
