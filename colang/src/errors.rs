//! Compilation error definitions.

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::ast;
use crate::ast::InputSpanFile;
use ast::InputSpan;
use ast::ParseError;

#[derive(Debug)]
pub struct CompilationError {
    pub code: &'static str,
    pub message: String,
    pub location: Option<InputSpan>,
}

impl CompilationError {
    /// Builds a `codespan_reporting` diagnostic.
    pub fn to_codespan<I>(&self, user_program_id: I, std_id: I) -> Diagnostic<I> {
        let mut result = Diagnostic::error()
            .with_message(&self.message)
            .with_code(self.code);

        if let Some(ref location) = self.location {
            let file_id = match location.file {
                InputSpanFile::UserProgram => user_program_id,
                InputSpanFile::Std => std_id,
            };
            result =
                result.with_labels(vec![Label::primary(file_id, location.start..location.end)]);
        }

        result
    }

    // Error definitions from this point onwards.

    pub fn syntax_error(err: ParseError, file: InputSpanFile) -> CompilationError {
        // TODO actually handle `expected` fields.
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
                "`{}` is not {}, but {}",
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

    pub fn read_unsupported_type(actual_type: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9010",
            message: format!(
                "can only read `int` and `string` variables, not `{}`",
                actual_type
            ),
            location: Some(location),
        }
    }

    pub fn assignment_target_not_lvalue(location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9011",
            message: "assignment target must be an lvalue".to_string(),
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
                "cannot assign value of type `{}` to a target of type `{}`",
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

    pub fn write_value_is_not_stringable(
        actual_type: &str,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9019",
            message: format!(
                "can only write values of type `string` or convertible to `string`, not `{}`",
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
                "function `{}` expects {} argument(s), not {} as given",
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

    pub fn array_elements_type_mismatch(
        inferred_type: &str,
        element_type: &str,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9023",
            message: format!(
                "array element has type `{}`, but it must have the same type `{}` as other elements",
                element_type,
                inferred_type
            ),
            location: Some(location),
        }
    }

    pub fn read_target_not_lvalue(location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9026",
            message: format!("`read` statement target must be an lvalue"),
            location: Some(location),
        }
    }

    pub fn array_size_not_int(actual_type: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9027",
            message: format!("array size must be of type `int`, not `{}`", actual_type),
            location: Some(location),
        }
    }

    pub fn cannot_infer_empty_type(location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9028",
            message: format!("empty array type cannot be inferred from context"),
            location: Some(location),
        }
    }

    pub fn address_of_rvalue(location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9029",
            message: format!("cannot take address of an rvalue"),
            location: Some(location),
        }
    }

    pub fn can_only_dereference_pointer(
        actual_type: &str,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9030",
            message: format!("can only dereference pointers, not `{}`", actual_type),
            location: Some(location),
        }
    }

    pub fn self_must_be_lvalue(method_name: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9031",
            message: format!("`self` must be an lvalue for method `{}`", method_name),
            location: Some(location),
        }
    }

    pub fn self_not_in_method_signature(
        function_name: &str,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9032",
            message: format!(
                "`self` can only appear as a parameter for methods, but `{}` is a function",
                function_name
            ),
            location: Some(location),
        }
    }

    pub fn self_in_function_body(location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9033",
            message: format!("`self` is not defined outside of methods"),
            location: Some(location),
        }
    }

    pub fn self_is_not_first_parameter(location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9034",
            message: format!("`self` must be the first parameter of the method"),
            location: Some(location),
        }
    }

    pub fn method_first_parameter_is_not_self(location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9035",
            message: format!("first parameter for methods must be `self`"),
            location: Some(location),
        }
    }

    pub fn literal_not_utf8(location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9036",
            message: format!("literal must be a valid UTF-8 sequence"),
            location: Some(location),
        }
    }

    pub fn char_literal_bad_length(actual_len: usize, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9037",
            message: format!(
                "`char` literals must contain exactly one UTF-8 byte, not {}",
                actual_len
            ),
            location: Some(location),
        }
    }

    pub fn unknown_escape_sequence(sequence: &str, location: InputSpan) -> CompilationError {
        CompilationError {
            code: "E9038",
            message: format!("unknown escape sequence: {}", sequence),
            location: Some(location),
        }
    }

    pub fn index_method_returns_not_pointer(
        type_name: &str,
        actual_return_type: &str,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9039",
            message: format!("`index` method must return a pointer in order to be used in indexing expressions, but for type `{}` it returns `{}`",
                type_name,
                actual_return_type,
            ),
            location: Some(location),
        }
    }

    pub fn binary_operator_unsupported_types(
        operator: &str,
        lhs_type: &str,
        rhs_type: &str,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9040",
            message: format!(
                "operator `{}` cannot be used with types `{}` and `{}`",
                operator, lhs_type, rhs_type,
            ),
            location: Some(location),
        }
    }

    pub fn wrong_number_of_type_template_arguments(
        template_name: &str,
        expected_num: usize,
        actual_num: usize,
        location: InputSpan,
    ) -> CompilationError {
        CompilationError {
            code: "E9041",
            message: format!(
                "type template `{}` requires {} type arguments, but {} is/are provided",
                template_name, expected_num, actual_num
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
    TypeTemplate,
    Field,
    Method,
}

impl Word {
    fn text(&self) -> &'static str {
        use Word::*;
        match self {
            Variable => "variable",
            Function => "function",
            Type => "type",
            TypeTemplate => "type template",
            Field => "field",
            Method => "method",
        }
    }

    fn indefinite_article(&self) -> &'static str {
        use Word::*;
        match self {
            Variable | Function | Type | TypeTemplate | Field | Method => "a",
        }
    }

    fn text_with_indefinite_article(&self) -> String {
        format!("{} {}", self.indefinite_article(), self.text())
    }
}
