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

    pub fn variable_type_omitted(variable_name: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9004",
            message: format!(
                "variable `{}` can't be declared without either a type or an initializer expression",
                variable_name
            ),
            location: Some(location),
        }
    }

    pub fn symbol_kind_mismatch(
        name: &str,
        expected: Word,
        actual: Word,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9005",
            message: format!(
                "`{}` is not {}, but a {}",
                name,
                expected.text_with_indefinite_article(),
                actual.text_with_indefinite_article(),
            ),
            location: Some(location),
        }
    }

    pub fn symbol_not_found(name: &str, kind: Word, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9006",
            message: format!(
                "no {} named `{}` could be found in the current scope",
                kind.text(),
                name
            ),
            location: Some(location),
        }
    }

    // TODO: add previous declaration site note.
    pub fn symbol_already_exists(name: &str, kind: Word, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9007",
            message: format!(
                "{} with name `{}` is already defined in this scope",
                kind.text(),
                name
            ),
            location: Some(location),
        }
    }

    pub fn condition_is_not_bool(actual_type: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9008",
            message: format!("condition must have type `bool`, not `{}`", actual_type),
            location: Some(location),
        }
    }

    pub fn operand_is_not_int(actual_type: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9009",
            message: format!("operand must have type `int`, not `{}`", actual_type),
            location: Some(location),
        }
    }

    pub fn read_target_not_int(actual_type: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9010",
            message: format!(
                "can only read variables of type `int`, not `{}`",
                actual_type
            ),
            location: Some(location),
        }
    }
}

/// Words that are commonly used as parameters for generic error types.
pub enum Word {
    Variable,
    Type,
}

impl Word {
    fn text(&self) -> &'static str {
        use Word::*;
        match self {
            Variable => "variable",
            Type => "type",
        }
    }

    fn indefinite_article(&self) -> &'static str {
        use Word::*;
        match self {
            Variable | Type => "a",
        }
    }

    fn text_with_indefinite_article(&self) -> String {
        format!("{} {}", self.indefinite_article(), self.text())
    }
}
