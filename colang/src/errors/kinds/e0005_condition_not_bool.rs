use crate::errors::CompilationError;
use crate::program::Type;
use crate::source::SourceOrigin;

pub fn condition_not_bool(actual_type: &Type, location: SourceOrigin) -> CompilationError {
    CompilationError::new(
        "E0005",
        format!(
            "condition must have type `bool`, not `{}`",
            actual_type.name
        ),
    )
    .with_location(location)
    .with_subtitle(format!(
        "has type `{}`, but only `bool` can be used here",
        actual_type.name
    ))
}
