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

    pub fn named_entity_kind_mismatch(
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

    pub fn named_entity_not_found(name: &str, kind: Word, location: InputSpan) -> CompilationError {
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
    pub fn named_entity_already_exists(
        name: &str,
        kind: Word,
        location: InputSpan,
    ) -> CompilationError {
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

    pub fn assignment_target_not_variable(location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9011",
            message: "assignment target must be a variable".to_string(),
            location: Some(location),
        }
    }

    pub fn assignment_type_mismatch(
        target_type: &str,
        value_type: &str,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9012",
            message: format!(
                "cannot assign value of type `{}` to a variable of type `{}`",
                value_type, target_type
            ),
            location: Some(location),
        }
    }

    pub fn main_function_not_found() -> CompilationError {
        CompilationError {
            code: "E9013",
            message: "`main` function not found: you must define one".to_string(),
            location: None,
        }
    }

    pub fn variable_of_type_void(location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9014",
            message: format!("variables are not allowed to have type `void`"),
            location: Some(location),
        }
    }

    pub fn function_body_type_mismatch(
        return_type: &str,
        body_type: &str,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9015",
            message: format!(
                "function is expected to return type `{}`, but its body has type `{}`",
                return_type, body_type,
            ),
            location: Some(location),
        }
    }

    pub fn while_body_not_void(actual_type: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9016",
            message: format!(
                "`while` loop body must have type `void`, not `{}`",
                actual_type
            ),
            location: Some(location),
        }
    }

    pub fn if_expression_missing_else(then_type: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9017",
            message: format!(
                "`if` expression without `else` branch can only be `void`, not `{}`",
                then_type
            ),
            location: Some(location),
        }
    }

    pub fn if_expression_branch_type_mismatch(
        then_type: &str,
        else_type: &str,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9018",
            message: format!(
                "`if` expression branches must have the same type, but are different: `{}` and `{}`",
                then_type,
                else_type,
            ),
            location: Some(location),
        }
    }

    pub fn write_value_not_int(actual_type: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9019",
            message: format!(
                "can only print variables of type `int`, not `{}`",
                actual_type
            ),
            location: Some(location),
        }
    }

    pub fn call_wrong_number_of_arguments(
        function_name: &str,
        expected: usize,
        actual: usize,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9020",
            message: format!(
                "function `{}` expects `{}` argument(s), not `{}` as given",
                function_name, expected, actual
            ),
            location: Some(location),
        }
    }

    pub fn call_argument_type_mismatch(
        parameter_name: &str,
        expected_type: &str,
        actual_type: &str,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9021",
            message: format!(
                "cannot pass a value of type `{}` as an argument for parameter `{}` of type `{}`",
                actual_type, parameter_name, expected_type
            ),
            location: Some(location),
        }
    }
}

/// Words that are commonly used as parameters for generic error types.
pub enum Word {
    Variable,
    Function,
    Type,
}

impl Word {
    fn text(&self) -> &'static str {
        use Word::*;
        match self {
            Variable => "variable",
            Function => "function",
            Type => "type",
        }
    }

    fn indefinite_article(&self) -> &'static str {
        use Word::*;
        match self {
            Variable | Function | Type => "a",
        }
    }

    fn text_with_indefinite_article(&self) -> String {
        format!("{} {}", self.indefinite_article(), self.text())
    }
}
