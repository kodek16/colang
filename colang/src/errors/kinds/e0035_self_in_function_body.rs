use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn self_in_function_body(location: SourceOrigin) -> CompilationError {
    CompilationError::new("E0035", "`self` is not defined outside of methods")
        .with_location(location)
        .with_subtitle("`self` is not defined here")
}
