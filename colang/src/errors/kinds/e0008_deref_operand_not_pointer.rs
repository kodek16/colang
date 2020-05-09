use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};

pub fn deref_operand_not_pointer(expression: &Expression) -> CompilationError {
    let actual_type = expression.type_().borrow();

    CompilationError::new(
        "E0008",
        format!("can only dereference pointers, not `{}`", actual_type.name),
    )
    .with_location(expression.location())
    .with_subtitle(format!("has a non-pointer type `{}`", actual_type.name))
    .maybe_with_type_explanation(expression)
}
