use super::compile_expression;
use crate::analyzer::bodies::maybe_deref;
use crate::context::CompilerContext;
use crate::source::SourceOrigin;
use crate::{ast, program};

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

    let field = receiver_type.borrow().lookup_field(&expression.field.text);
    let field = match field {
        Ok(field) => field,
        Err(error) => {
            let error = error.into_direct_lookup_error(SourceOrigin::Plain(expression.field.span));
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    program::Expression::new(
        program::FieldAccessExpr {
            receiver: Box::new(receiver),
            field,
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
