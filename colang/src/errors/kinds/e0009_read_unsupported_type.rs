use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};

pub fn read_unsupported_type(target: &Expression) -> CompilationError {
    let actual_type = target.type_().borrow();

    CompilationError::new(
        "E0009",
        format!(
            "can only read `int` and `string` values, not `{}`",
            actual_type.name
        ),
    )
    .with_location(target.location())
    .with_subtitle(format!(
        "target has type `{}` that does not support reading",
        actual_type.name
    ))
    .maybe_with_type_explanation(target)
}
