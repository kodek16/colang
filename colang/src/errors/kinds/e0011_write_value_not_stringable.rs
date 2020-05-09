use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};

pub fn write_value_not_stringable(value: &Expression) -> CompilationError {
    CompilationError::new(
        "E0011",
        format!(
            "can only write values of type `string` or convertible to `string`, not `{}`",
            value.type_().borrow().name
        ),
    )
    .with_location(value.location())
    .with_subtitle(format!("has type `{}`", value.type_().borrow().name))
    .maybe_with_type_explanation(value)
}
