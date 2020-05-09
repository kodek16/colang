use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};

pub fn array_size_not_int(size: &Expression) -> CompilationError {
    let size_type = size.type_().borrow();

    CompilationError::new(
        "E0006",
        format!("array size must be of type `int`, not `{}`", size_type.name),
    )
    .with_location(size.location())
    .with_subtitle(format!("size has type `{}`", size_type.name))
    .maybe_with_type_explanation(size)
}
