use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn statement_used_as_expression(location: SourceOrigin) -> CompilationError {
    CompilationError::new("E0057", "statement cannot be used as expression")
        .with_location(location)
        .with_subtitle("this is a statement, but an expression was expected")
}
