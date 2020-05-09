use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn self_is_not_first_parameter(location: SourceOrigin) -> CompilationError {
    CompilationError::new("E0036", "`self` must be the first parameter of a method")
        .with_location(location)
        .with_subtitle("`self` not allowed here")
}
