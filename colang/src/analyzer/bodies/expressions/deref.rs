use super::compile_expression;
use crate::errors::CompilationError;
use crate::program::Type;
use crate::source::SourceOrigin;
use crate::{ast, program, CompilerContext};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_deref_expr(
    expression: ast::DerefExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let hint = type_hint.map(|hint| context.program.types_mut().pointer_to(&hint));

    let pointer = compile_expression(*expression.pointer, hint, context);

    if pointer.type_().borrow().is_error() {
        return program::Expression::error(expression.span);
    }

    if !pointer.type_().borrow().is_pointer() {
        let error = CompilationError::can_only_dereference_pointer(&pointer);
        context.errors.push(error);
        return program::Expression::error(expression.span);
    }

    program::Expression::new(
        program::ExpressionKind::Deref(program::DerefExpr {
            pointer: Box::new(pointer),
            location: SourceOrigin::Plain(expression.span),
        }),
        context.program.types_mut(),
    )
}
