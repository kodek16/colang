use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};

pub fn logical_operator_operand_wrong_type(
    operator: &str,
    operand: &Expression,
) -> CompilationError {
    let actual_type = operand.type_().borrow();

    CompilationError::new(
        "E0043",
        format!(
            "operand of logical operator `{}` must have type `bool`, not `{}`",
            operator, actual_type.name
        ),
    )
    .with_location(operand.location())
    .with_subtitle(format!("operand has type `{}`", actual_type.name))
    .maybe_with_type_explanation(operand)
}
