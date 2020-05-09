use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};

pub fn is_expression_operand_wrong_type(operand: &Expression) -> CompilationError {
    let actual_type = operand.type_().borrow();

    CompilationError::new(
        "E0044",
        format!(
            "only pointers can be compared using `is`, but operand has type `{}`",
            actual_type.name
        ),
    )
    .with_location(operand.location())
    .with_subtitle(format!("operand has type `{}`", actual_type.name))
    .maybe_with_type_explanation(operand)
}
