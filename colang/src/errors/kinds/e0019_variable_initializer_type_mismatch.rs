use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, Variable};

pub fn variable_initializer_type_mismatch(
    variable: &Variable,
    initializer: &Expression,
) -> CompilationError {
    let variable_type = variable.type_.borrow();
    let initializer_type = initializer.type_().borrow();

    // TODO point variable note to type and not the whole definition.

    CompilationError::new(
        "E0019",
        format!(
            "cannot initialize a variable of type `{}` with a value of type `{}`",
            variable_type.name, initializer_type.name,
        ),
    )
    .with_location(initializer.location())
    .with_subtitle(format!(
        "initializer has type `{}`, expected `{}`",
        initializer_type.name, variable_type.name,
    ))
    .maybe_with_type_explanation(initializer)
    .with_bound_note(
        variable.definition_site.unwrap(),
        format!("variable defined with type `{}`", variable_type.name),
    )
}
