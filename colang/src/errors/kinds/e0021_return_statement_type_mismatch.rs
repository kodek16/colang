use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, Function};

pub fn return_statement_type_mismatch(
    function: &Function,
    expression: &Expression,
) -> CompilationError {
    let expression_type = expression.type_().borrow();
    let return_type = function.return_type.borrow();

    CompilationError::new(
        "E0021",
        format!(
            "cannot return a value of type `{}` from a function that returns `{}`",
            expression_type.name, return_type.name
        ),
    )
    .with_location(expression.location())
    .with_subtitle(format!("has type `{}`", expression_type.name))
    .maybe_with_type_explanation(expression)
    .with_bound_note(
        // TODO(#2) point at return type exactly, and not at the whole signature.
        function.definition_site.unwrap(),
        format!(
            "function is defined with return type `{}`",
            return_type.name
        ),
    )
}
