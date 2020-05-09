use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn variable_no_type_or_initializer(
    variable_name: &str,
    location: SourceOrigin,
) -> CompilationError {
    CompilationError::new(
        "E0013",
        format!(
            "variable `{}` cannot be defined with no type or initializer expression",
            variable_name
        ),
    )
    .with_location(location)
    .with_subtitle(format!("type of `{}` is unknown here", variable_name))
    .with_bound_note(
        location,
        format!(
            "help: try adding a type, for example `{}: int`",
            variable_name
        ),
    )
}
