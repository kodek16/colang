use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn char_literal_bad_length(actual_len: usize, location: SourceOrigin) -> CompilationError {
    CompilationError::new(
        "E0039",
        format!(
            "`char` literals must contain exactly one UTF-8 byte, not {}",
            actual_len
        ),
    )
    .with_location(location)
    .with_subtitle(format!("literal length is {} bytes in UTF-8", actual_len))
}
