use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, Variable};

pub fn call_argument_type_mismatch(
    argument: &Expression,
    parameter: &Variable,
) -> CompilationError {
    CompilationError::new(
        "E0024",
        format!(
            "cannot pass a value of type `{}` as an argument for parameter `{}` of type `{}`",
            argument.type_().borrow().name,
            parameter.name,
            parameter.type_.borrow().name
        ),
    )
    .with_location(argument.location())
    .with_subtitle(format!(
        "argument has type `{}`",
        argument.type_().borrow().name
    ))
    .maybe_with_type_explanation(argument)
    .maybe_with_bound_note(parameter.definition_site, || {
        format!(
            "parameter `{}` is defined here with type `{}`",
            parameter.name,
            parameter.type_.borrow().name
        )
    })
}
