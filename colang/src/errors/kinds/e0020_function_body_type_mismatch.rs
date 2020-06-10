use crate::errors::CompilationError;
use crate::program::{Expression, Function};

// `function` is assumed to be non-void.
pub fn function_body_type_mismatch(function: &Function, body: &Expression) -> CompilationError {
    let return_type = function.return_type.as_ref().unwrap().borrow();

    CompilationError::new(
        "E0020",
        format!(
            "function is expected to return type `{}`, but its body has type `{}`",
            return_type.name,
            body.type_().borrow().name,
        ),
    )
    // TODO(#2) point at return type exactly, and not at the whole signature.
    .with_location(function.definition_site.unwrap())
    .with_subtitle(format!(
        "signature specifies return type as `{}`",
        return_type.name
    ))
    .maybe_with_type_explanation(body)
}
