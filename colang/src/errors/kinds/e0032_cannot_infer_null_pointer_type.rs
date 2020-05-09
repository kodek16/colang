use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn cannot_infer_null_pointer_type(location: SourceOrigin) -> CompilationError {
    CompilationError::new(
        "E0032",
        "`null` pointer type cannot be inferred from context",
    )
    .with_location(location)
    .with_subtitle("pointer type is unknown")
}
