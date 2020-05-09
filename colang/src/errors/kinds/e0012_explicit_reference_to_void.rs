use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn explicit_reference_to_void(location: SourceOrigin) -> CompilationError {
    CompilationError::new("E0012", "cannot explicitly refer to `void` type")
        .with_location(location)
        .with_subtitle("`void` type cannot be used explicitly")
}
