use super::compile_expression;
use crate::context::CompilerContext;
use crate::errors;
use crate::source::SourceOrigin;
use crate::{ast, program};
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

    if !size.type_().borrow().is_int() {
        let error = errors::array_size_not_int(&size);
        context.errors.push(error);
    }

    program::Expression::new(
        program::ArrayFromCopyExpr {
            element: Box::new(element),
            size: Box::new(size),
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
