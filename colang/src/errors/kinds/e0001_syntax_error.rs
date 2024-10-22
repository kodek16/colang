use crate::ast::ParseError;
use crate::errors::CompilationError;
use crate::source::{InputSpan, InputSpanFile, SourceOrigin};

pub fn syntax_error(err: ParseError, file: InputSpanFile) -> CompilationError {
    let (message, location) = match err {
        ParseError::User { .. } => panic!("Unexpected custom parse error."),
        ParseError::InvalidToken { location } => (
            "Invalid token",
            InputSpan {
                file,
                start: location,
                end: location + 1,
            },
        ),
        ParseError::UnrecognizedEOF { location, .. } => (
            "Unrecognized EOF",
            InputSpan {
                file,
                start: location,
                end: location + 1,
            },
        ),
        ParseError::UnrecognizedToken {
            token: (start, _, end),
            ..
        } => ("Unrecognized token", InputSpan { file, start, end }),
        ParseError::ExtraToken {
            token: (start, _, end),
        } => ("Extra token", InputSpan { file, start, end }),
    };

    CompilationError::new("E0001", message).with_location(SourceOrigin::Plain(location))
}
