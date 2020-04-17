use super::compile_expression;
use crate::errors::CompilationError;
use crate::program::ExpressionKind;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_array_from_copy_expr(
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

    let size_type = size.type_();
    if *size_type != *context.program.types().int() {
        let error = CompilationError::array_size_not_int(
            size_type.borrow().name(),
            size.span().expect("Generated array size is not int"),
        );
        context.errors.push(error);
    }

    let kind = ExpressionKind::ArrayFromCopy(program::ArrayFromCopyExpr {
        element: Box::new(element),
        size: Box::new(size),
        span: Some(expression.span),
    });

    program::Expression::new(kind, context.program.types_mut())
}
