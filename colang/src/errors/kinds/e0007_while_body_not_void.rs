use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind};

pub fn while_body_not_void(body: &Expression) -> CompilationError {
    CompilationError::new(
        "E0007",
        format!(
            "`while` loop body must have type `void`, not `{}`",
            body.type_().borrow().name
        ),
    )
    .with_location(body.location())
    .with_subtitle(format!("body has type `{}`", body.type_().borrow().name))
    .maybe_with_type_explanation(body)
}
