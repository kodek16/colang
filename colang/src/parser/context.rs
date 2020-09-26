use crate::source::InputSpanFile;

/// Context passed around between all parsing routines.
pub struct ParsingContext {
    pub file: InputSpanFile,
}
