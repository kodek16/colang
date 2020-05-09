use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};
use crate::source::SourceOrigin;

pub fn assignment_type_mismatch(
    target: &Expression,
    value: &Expression,
    assignment_location: SourceOrigin,
) -> CompilationError {
    let target_type = target.type_().borrow();
    let value_type = value.type_().borrow();

    CompilationError::new(
        "E0018",
        format!(
            "cannot assign value of type `{}` to a target of type `{}`",
            value_type.name, target_type.name
        ),
    )
    .with_location(assignment_location)
    .with_subtitle("types must be the same")
    .with_bound_note(
        target.location(),
        format!("target has type `{}`", target_type.name),
    )
    .with_bound_note(
        value.location(),
        format!("value has type `{}`", value_type.name),
    )
    .maybe_with_type_explanation(value)
}
