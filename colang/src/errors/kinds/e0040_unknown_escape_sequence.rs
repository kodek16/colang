use crate::errors::CompilationError;
use crate::source::SourceOrigin;

pub fn unknown_escape_sequence(sequence: &str, location: SourceOrigin) -> CompilationError {
    CompilationError::new(
        "E0040",
        format!("unknown escape sequence: \"{}\"", sequence),
    )
    .with_location(location)
    .with_subtitle(format!(
        "literal contains unknown escape sequence \"{}\"",
        sequence
    ))
}
