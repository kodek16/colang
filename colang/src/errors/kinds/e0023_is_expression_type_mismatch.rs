use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};
use crate::source::SourceOrigin;

pub fn is_expression_type_mismatch(
    lhs: &Expression,
    rhs: &Expression,
    location: SourceOrigin,
) -> CompilationError {
    let lhs_type = lhs.type_().borrow();
    let rhs_type = rhs.type_().borrow();

    CompilationError::new(
        "E0023",
        format!(
            "operands to `is` must have same type, not different `{}` and `{}`",
            lhs_type.name, rhs_type.name,
        ),
    )
    .with_location(location)
    .with_subtitle("types must be the same")
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
