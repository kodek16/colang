use crate::errors::CompilationError;
use crate::source::SourceOrigin;

// TODO: test this error once `\xdd` escapes are implemented and non UTF-8 literals are
// actually possible.
pub fn literal_not_utf8(location: SourceOrigin) -> CompilationError {
    CompilationError::new("E0038", "literal must be a valid UTF-8 sequence")
        .with_location(location)
        .with_subtitle("string is not valid UTF-8")
}
