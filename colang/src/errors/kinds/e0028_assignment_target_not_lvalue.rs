use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn assignment_target_not_lvalue(location: SourceOrigin) -> CompilationError {
    CompilationError::new("E0028", "assignment target must be an lvalue")
        .with_location(location)
        .with_subtitle("expression is an rvalue, but only lvalues can be assigned to")
}
