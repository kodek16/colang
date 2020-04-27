use super::compile_expression;
use crate::errors::CompilationError;
use crate::program::ExpressionKind;
use crate::source::SourceOrigin;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub(crate) fn compile_array_from_copy_expr(
    expression: ast::ArrayFromCopyExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let element = compile_expression(*expression.element, None, context);
    let size = compile_expression(
        *expression.size,
        Some(Rc::clone(context.program.types().int())),
        context,
    );
    if element.is_error() || size.is_error() {
        return program::Expression::error(expression.span);
    }

    if size.type_() != context.program.types().int() {
        let error = CompilationError::array_size_not_int(&size);
        context.errors.push(error);
    }

    let kind = ExpressionKind::ArrayFromCopy(program::ArrayFromCopyExpr {
        element: Box::new(element),
        size: Box::new(size),
        location: SourceOrigin::Plain(expression.span),
    });

    program::Expression::new(kind, context.program.types_mut())
}
