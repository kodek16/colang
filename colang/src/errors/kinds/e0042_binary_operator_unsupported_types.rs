use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};
use crate::source::SourceOrigin;

pub fn binary_operator_unsupported_types(
    operator: &str,
    lhs: &Expression,
    rhs: &Expression,
    location: SourceOrigin,
) -> CompilationError {
    let lhs_type = lhs.type_().borrow();
    let rhs_type = rhs.type_().borrow();

    CompilationError::new(
        "E0042",
        format!(
            "operator `{}` cannot be used with types `{}` and `{}`",
            operator, lhs_type.name, rhs_type.name,
        ),
    )
    .with_location(location)
    .with_subtitle(format!(
        "operator `{}` is not defined for `{}` and `{}`",
        operator, lhs_type.name, rhs_type.name
    ))
    .with_bound_note(
        lhs.location(),
        format!("left operand has type `{}`", lhs_type.name),
    )
    .maybe_with_type_explanation(lhs)
    .with_bound_note(
        rhs.location(),
        format!("right operand has type `{}`", rhs_type.name),
    )
    .maybe_with_type_explanation(rhs)
}
