use crate::errors::CompilationError;
use crate::source::SourceOrigin;

// TODO(#4): remove this once associated functions are introduced
pub fn method_first_parameter_is_not_self(signature: SourceOrigin) -> CompilationError {
    CompilationError::new("E0037", "first parameter for methods must be `self`")
        .with_location(signature)
}
