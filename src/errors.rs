//! Compilation error definitions.

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::ast;
use ast::InputSpan;
use ast::ParseError;

#[derive(Debug)]
pub struct CompilationError {
    code: &'static str,
    message: String,
    location: Option<InputSpan>,
}

impl CompilationError {
    /// Builds a `codespan_reporting` diagnostic.
    pub fn to_codespan(&self) -> Diagnostic<()> {
        let mut result = Diagnostic::error()
            .with_message(&self.message)
            .with_code(self.code);

        if let Some(ref location) = self.location {
            result = result.with_labels(vec![Label::primary((), location.start..location.end)]);
        }

        result
    }

    // Error definitions from this point onwards.

    pub fn syntax_error(err: ParseError) -> CompilationError {
        // TODO actually handle `expected` fields.
        let (message, location) = match err {
            ParseError::User { .. } => panic!("Unexpected custom parse error."),
            ParseError::InvalidToken { location } => (
                "Invalid token",
                InputSpan {
                    start: location,
                    end: location + 1,
                },
            ),
            ParseError::UnrecognizedEOF { location, .. } => (
                "Unrecognized EOF",
                InputSpan {
                    start: location,
                    end: location + 1,
                },
            ),
            ParseError::UnrecognizedToken {
                token: (start, _, end),
                ..
            } => ("Unrecognized token", InputSpan { start, end }),
            ParseError::ExtraToken {
                token: (start, _, end),
            } => ("Extra token", InputSpan { start, end }),
        };

        CompilationError {
            code: "E9000",
            message: message.to_string(),
            location: Some(location),
        }
    }

    // TODO: add previous declaration site note.
    pub fn variable_already_exists(name: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9001",
            message: format!(
                "variable with name `{}` is already defined in this scope",
                name
            ),
            location: Some(location),
        }
    }

    pub fn variable_not_found(name: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9002",
            message: format!("no variable named `{}` could be found in this scope", name),
            location: Some(location),
        }
    }
}
