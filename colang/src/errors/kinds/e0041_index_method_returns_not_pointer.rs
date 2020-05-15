use crate::errors::CompilationError;
use crate::source::SourceOrigin;

// TODO(#6): remove this when index support is implemented via trait.
pub fn index_method_returns_not_pointer(
    type_name: &str,
    actual_return_type: &str,
    location: SourceOrigin,
) -> CompilationError {
    CompilationError::new(
        "E0041",
        format!("`index` method must return a pointer in order to be used in indexing expressions, but for type `{}` it returns `{}`",
                type_name,
                actual_return_type,
        ))
        .with_location(location)
}
