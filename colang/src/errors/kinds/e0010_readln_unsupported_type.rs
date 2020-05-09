use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};

pub fn readln_unsupported_type(target: &Expression) -> CompilationError {
    let actual_type = target.type_().borrow();

    CompilationError::new(
        "E0010",
        format!(
            "`readln` can be only used with strings, not `{}`",
            actual_type.name
        ),
    )
    .with_location(target.location())
    .with_subtitle(format!(
        "target has type `{}` that does not support reading by line",
        actual_type.name
    ))
    .maybe_with_type_explanation(target)
}
