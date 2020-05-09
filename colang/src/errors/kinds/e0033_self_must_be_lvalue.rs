use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, Function};

pub fn self_must_be_lvalue(receiver: &Expression, method: &Function) -> CompilationError {
    CompilationError::new(
        "E0033",
        format!("`self` must be an lvalue for method `{}`", method.name),
    )
    .with_location(receiver.location())
    .with_subtitle("`self` is an rvalue here")
    .maybe_with_bound_note(
        method
            .parameters
            .get(0)
            .and_then(|self_| self_.borrow().definition_site),
        || {
            format!(
                "method `{}` takes `self` by pointer, so it needs to be an lvalue",
                method.name
            )
        },
    )
}
