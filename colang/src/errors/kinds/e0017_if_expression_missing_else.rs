use crate::errors::CompilationError;
use crate::source::SourceOrigin;

// TODO: remove after void-sanity is achieved
pub fn if_expression_missing_else(then_type: &str, location: SourceOrigin) -> CompilationError {
    CompilationError::new(
        "E0017",
        format!(
            "`if` expression without `else` branch can only be `void`, not `{}`",
            then_type
        ),
    )
    .with_location(location)
}
