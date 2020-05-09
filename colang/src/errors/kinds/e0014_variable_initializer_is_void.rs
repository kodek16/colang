use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};

pub fn variable_initializer_is_void(
    variable_name: &str,
    initializer: &Expression,
) -> CompilationError {
    CompilationError::new(
        "E0014",
        format!(
            "cannot initialize variable `{}` with a value of type `void`",
            variable_name
        ),
    )
    .with_location(initializer.location())
    .with_subtitle("has type `void`, so cannot be used as an initializer")
    .maybe_with_type_explanation(initializer)
}
