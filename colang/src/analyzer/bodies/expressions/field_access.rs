use super::compile_expression;
use crate::analyzer::bodies::maybe_deref;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_field_access_expr(
    expression: ast::FieldAccessExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let receiver = compile_expression(*expression.receiver, None, context);
    if receiver.is_error() {
        return program::Expression::error(expression.span);
    }

    // Automatically dereference pointers.
    let receiver = maybe_deref(receiver, context);

    let receiver_type = receiver.type_();

    let field = receiver_type
        .borrow()
        .lookup_field(&expression.field.text, expression.field.span)
        .map(Rc::clone);

    let field = match field {
        Ok(field) => field,
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    program::FieldAccessExpr::new(
        receiver,
        Rc::clone(&field),
        context.program.types_mut(),
        expression.span,
    )
}
