use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn cannot_infer_empty_array_type(location: SourceOrigin) -> CompilationError {
    CompilationError::new("E0031", "empty array type cannot be inferred from context")
        .with_location(location)
        .with_subtitle("array type is unknown")
}
