//! Compilation error definitions.

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::ast;
use crate::ast::InputSpanFile;
use crate::program::{Expression, ExpressionKind, Function, SourceOrigin, Type};
use crate::scope::{NamedEntity, NamedEntityKind};
use ast::InputSpan;
use ast::ParseError;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct CompilationError {
    /// Unique code identifying a class of errors.
    pub code: &'static str,

    /// Short (single-line) message describing the problem.
    pub message: String,

    /// Location in code where the problem occurred.
    pub location: Option<InputSpan>,

    /// Extended description of the problem that is shown directly next to code.
    pub subtitle: Option<String>,

    /// Useful information related to the error, bound to some location in code.
    pub bound_notes: Vec<(InputSpan, String)>,

    /// Useful information related to the error, not bound to any location.
    pub free_notes: Vec<String>,
}

impl CompilationError {
    fn new(code: &'static str, message: String, location: SourceOrigin) -> CompilationError {
        let (location, location_note) = match location {
            SourceOrigin::Plain(span) => (span, None),
            SourceOrigin::AutoDeref(span) => {
                (span, Some("expression was automatically dereferenced"))
            }
            SourceOrigin::DereferencedIndex(span) => (
                span,
                Some("`index` method return value was automatically dereferenced"),
            ),
            SourceOrigin::AddressedForRead(span) => (
                span,
                Some("expression address was automatically taken for reading into"),
            ),
            SourceOrigin::ReadFunctionCall(span) => (
                span,
                Some("internal function call was automatically generated for `read`"),
            ),
            SourceOrigin::Stringified(span) => (
                span,
                Some("expression was automatically converted to `string`"),
            ),
            SourceOrigin::AddressedForMethodCall(span) => (
                span,
                Some("reference to method receiver was automatically taken"),
            ),
            SourceOrigin::MissingElse(span) => {
                (span, Some("`else` branch of an `if` expression is missing"))
            }
            SourceOrigin::MissingBlockValue(span) => (
                span,
                Some("block has type `void` because it is does not contain a finishing expression"),
            ),
        };

        let location_note = location_note.map(|note| (location, note.to_string()));

        CompilationError {
            code,
            message,
            location: Some(location),
            subtitle: None,
            bound_notes: location_note.into_iter().collect(),
            free_notes: Vec::new(),
        }
    }
}

impl CompilationError {
    /// Builds a `codespan_reporting` diagnostic.
    pub fn to_codespan<I: Copy>(&self, user_program_id: I, std_id: I) -> Diagnostic<I> {
        let mut labels = Vec::new();

        if let Some(ref location) = self.location {
            let file_id = match location.file {
                InputSpanFile::UserProgram => user_program_id,
                InputSpanFile::Std => std_id,
            };

            if let Some(ref subtitle) = self.subtitle {
                labels.push(
                    Label::primary(file_id, location.start..location.end).with_message(subtitle),
                );
            } else {
                labels.push(Label::primary(file_id, location.start..location.end));
            }
        }

        for (location, note) in &self.bound_notes {
            let file_id = match location.file {
                InputSpanFile::UserProgram => user_program_id,
                InputSpanFile::Std => std_id,
            };
            labels.push(Label::secondary(file_id, location.start..location.end).with_message(note));
        }

        Diagnostic::error()
            .with_code(self.code)
            .with_message(&self.message)
            .with_labels(labels)
            .with_notes(self.free_notes.clone())
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

        CompilationError::new("E9000", message.to_string(), SourceOrigin::Plain(location))
    }

    pub fn variable_no_type_or_initializer(
        variable_name: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        let mut error = CompilationError::new(
            "E9004",
            format!(
                "variable `{}` cannot be defined with no type or initializer expression",
                variable_name
            ),
            location,
        );

        error.subtitle = Some(format!("type of `{}` is unknown here", variable_name));
        error.bound_notes.push((
            location.as_plain(),
            format!(
                "help: try adding a type, for example `{}: int`",
                variable_name
            ),
        ));

        error
    }

    pub fn named_entity_not_found(
        name: &str,
        kind: NamedEntityKind,
        location: SourceOrigin,
    ) -> CompilationError {
        let mut error = CompilationError::new(
            "E9006",
            format!(
                "no {} with name `{}` could be found in the current scope",
                kind.text(),
                name
            ),
            location,
        );
        error.subtitle = Some(format!("refers to an unknown {}", kind.text()));
        error
    }

    pub fn named_entity_kind_mismatch(
        name: &str,
        expected: NamedEntityKind,
        actual: &NamedEntity,
        location: SourceOrigin,
    ) -> CompilationError {
        let mut error = CompilationError::new(
            "E9005",
            format!(
                "expected `{}` to be {}, but it is {}",
                name,
                actual.kind().text_with_indefinite_article(),
                expected.text_with_indefinite_article(),
            ),
            location,
        );
        error.subtitle = Some(format!("`{}` is used here in a type context", name));

        if let Some(definition_site) = actual.definition_site() {
            error.bound_notes.push((
                definition_site.as_plain(),
                format!(
                    "`{}` is defined here as {}",
                    actual.name(),
                    actual.kind().text_with_indefinite_article(),
                ),
            ));
        }
        error
    }

    pub fn named_entity_already_defined(
        name: &str,
        existing: &NamedEntity,
        location: SourceOrigin,
    ) -> CompilationError {
        let mut error = CompilationError::new(
            "E9007",
            format!(
                "{} with name `{}` is already defined in this scope",
                existing.kind().text_with_indefinite_article(),
                name
            ),
            location,
        );

        error.subtitle = Some(format!("attempted to redefine `{}` here", name));
        if let Some(definition_site) = existing.definition_site() {
            error.bound_notes.push((
                definition_site.as_plain(),
                format!("`{}` previously defined here", name),
            ));
        }

        error
    }

    pub fn condition_is_not_bool(actual_type: &Type, location: SourceOrigin) -> CompilationError {
        let mut error = CompilationError::new(
            "E9008",
            format!(
                "condition must have type `bool`, not `{}`",
                actual_type.name
            ),
            location,
        );
        error.subtitle = Some(format!(
            "has type `{}`, but only `bool` can be used here",
            actual_type.name
        ));
        error
    }

    pub fn read_unsupported_type(expression: &Expression) -> CompilationError {
        let actual_type = expression.type_().borrow();
        let mut error = CompilationError::new(
            "E9010",
            format!(
                "can only read `int` and `string` values, not `{}`",
                actual_type.name
            ),
            expression.location(),
        );

        error.subtitle = Some(format!(
            "value has type `{}` that does not support reading",
            actual_type.name
        ));

        error
    }

    pub fn assignment_target_not_lvalue(location: SourceOrigin) -> CompilationError {
        let mut error = CompilationError::new(
            "E9011",
            "assignment target must be an lvalue".to_string(),
            location,
        );

        error.subtitle = Some(
            "expression evaluates to an rvalue, but only lvalues can be assigned to".to_string(),
        );

        error
    }

    pub fn assignment_type_mismatch(
        target: &Expression,
        value: &Expression,
        assignment_location: SourceOrigin,
    ) -> CompilationError {
        let target_type = target.type_().borrow();
        let value_type = value.type_().borrow();

        let mut error = CompilationError::new(
            "E9012",
            format!(
                "cannot assign value of type `{}` to a target of type `{}`",
                value_type.name, target_type.name
            ),
            assignment_location,
        );

        error.subtitle = Some("types must be the same".to_string());

        error.bound_notes.push((
            target.location().as_plain(),
            format!("target has type `{}`", target_type.name),
        ));
        error.bound_notes.push((
            value.location().as_plain(),
            format!("value has type `{}`", value_type.name),
        ));
        explain_expression_type(value, &mut error);

        error
    }

    pub fn main_function_not_found() -> CompilationError {
        CompilationError {
            code: "E9013",
            message: "`main` function not found: you must define one".to_string(),
            location: None,
            subtitle: None,
            bound_notes: Vec::new(),
            free_notes: Vec::new(),
        }
    }

    pub fn explicit_reference_to_void(location: SourceOrigin) -> CompilationError {
        let mut error = CompilationError::new(
            "E9014",
            "cannot explicitly refer to `void` type".to_string(),
            location,
        );
        error.subtitle = Some("`void` type cannot be used explicitly".to_string());
        error
    }

    pub fn variable_initializer_is_void(
        variable_name: &str,
        initializer: &Expression,
    ) -> CompilationError {
        let mut error = CompilationError::new(
            "E9049",
            format!(
                "cannot initialize variable `{}` with a value of type `void`",
                variable_name
            ),
            initializer.location(),
        );

        error.subtitle = Some("has type `void`, so cannot be used as an initializer".to_string());
        explain_expression_type(initializer, &mut error);

        error
    }

    pub fn function_body_type_mismatch(function: &Function, body: &Expression) -> CompilationError {
        let return_type = function.return_type.borrow();

        let mut error = CompilationError::new(
            "E9015",
            format!(
                "function is expected to return type `{}`, but its body has type `{}`",
                return_type.name,
                body.type_().borrow().name,
            ),
            // TODO point at return type exactly, and not at the whole signature.
            function.definition_site.unwrap(),
        );

        error.subtitle = Some(format!(
            "signature specifies return type as `{}`",
            return_type.name
        ));
        explain_expression_type(body, &mut error);

        error
    }

    pub fn while_body_not_void(actual_type: &str, location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9016",
            format!(
                "`while` loop body must have type `void`, not `{}`",
                actual_type
            ),
            location,
        )
    }

    pub fn if_expression_missing_else(then_type: &str, location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9017",
            format!(
                "`if` expression without `else` branch can only be `void`, not `{}`",
                then_type
            ),
            location,
        )
    }

    pub fn if_expression_branch_type_mismatch(
        then_type: &str,
        else_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9018",
            format!(
                "`if` expression branches must have the same type, but are different: `{}` and `{}`",
                then_type,
                else_type,
            ),
            location
        )
    }

    pub fn write_value_is_not_stringable(
        actual_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9019",
            format!(
                "can only write values of type `string` or convertible to `string`, not `{}`",
                actual_type
            ),
            location,
        )
    }

    pub fn call_wrong_number_of_arguments(
        function_name: &str,
        expected: usize,
        actual: usize,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9020",
            format!(
                "function `{}` expects {} argument(s), not {} as given",
                function_name, expected, actual
            ),
            location,
        )
    }

    pub fn call_argument_type_mismatch(
        parameter_name: &str,
        expected_type: &str,
        actual_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9021",
            format!(
                "cannot pass a value of type `{}` as an argument for parameter `{}` of type `{}`",
                actual_type, parameter_name, expected_type
            ),
            location,
        )
    }

    pub fn array_elements_type_mismatch(
        inferred_type: &str,
        element_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9023",
            format!(
                "array element has type `{}`, but it must have the same type `{}` as other elements",
                element_type,
                inferred_type
            ),
            location,
        )
    }

    pub fn read_target_not_lvalue(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9026",
            format!("`read` statement target must be an lvalue"),
            location,
        )
    }

    pub fn array_size_not_int(actual_type: &str, location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9027",
            format!("array size must be of type `int`, not `{}`", actual_type),
            location,
        )
    }

    pub fn cannot_infer_empty_type(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9028",
            format!("empty array type cannot be inferred from context"),
            location,
        )
    }

    pub fn address_of_rvalue(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9029",
            format!("cannot take address of an rvalue"),
            location,
        )
    }

    pub fn can_only_dereference_pointer(
        actual_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9030",
            format!("can only dereference pointers, not `{}`", actual_type),
            location,
        )
    }

    pub fn self_must_be_lvalue(method_name: &str, location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9031",
            format!("`self` must be an lvalue for method `{}`", method_name),
            location,
        )
    }

    pub fn self_not_in_method_signature(
        function_name: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9032",
            format!(
                "`self` can only appear as a parameter for methods, but `{}` is a function",
                function_name
            ),
            location,
        )
    }

    pub fn self_in_function_body(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9033",
            format!("`self` is not defined outside of methods"),
            location,
        )
    }

    pub fn self_is_not_first_parameter(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9034",
            format!("`self` must be the first parameter of the method"),
            location,
        )
    }

    pub fn method_first_parameter_is_not_self(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9035",
            format!("first parameter for methods must be `self`"),
            location,
        )
    }

    pub fn literal_not_utf8(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9036",
            format!("literal must be a valid UTF-8 sequence"),
            location,
        )
    }

    pub fn char_literal_bad_length(actual_len: usize, location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9037",
            format!(
                "`char` literals must contain exactly one UTF-8 byte, not {}",
                actual_len
            ),
            location,
        )
    }

    pub fn unknown_escape_sequence(sequence: &str, location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9038",
            format!("unknown escape sequence: {}", sequence),
            location,
        )
    }

    pub fn index_method_returns_not_pointer(
        type_name: &str,
        actual_return_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9039",
            format!("`index` method must return a pointer in order to be used in indexing expressions, but for type `{}` it returns `{}`",
                type_name,
                actual_return_type,
            ),
            location,
        )
    }

    pub fn binary_operator_unsupported_types(
        operator: &str,
        lhs_type: &str,
        rhs_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9040",
            format!(
                "operator `{}` cannot be used with types `{}` and `{}`",
                operator, lhs_type, rhs_type,
            ),
            location,
        )
    }

    pub fn wrong_number_of_type_template_arguments(
        template_name: &str,
        expected_num: usize,
        actual_num: usize,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9041",
            format!(
                "type template `{}` requires {} type arguments, but {} is/are provided",
                template_name, expected_num, actual_num
            ),
            location,
        )
    }

    pub fn new_expression_void_type(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9042",
            format!("cannot create an instance of type `void`"),
            location,
        )
    }

    pub fn type_infinite_dependency_chain(
        source_type: &Type,
        type_chain: Vec<Rc<RefCell<Type>>>,
        location: SourceOrigin,
    ) -> CompilationError {
        let mut result = CompilationError::new(
            "E9043",
            format!(
                "type `{}` causes an infinite type dependency chain through its fields and methods",
                source_type.name
            ),
            location,
        );

        let type_chain: Vec<_> = type_chain
            .iter()
            .map(|type_| type_.borrow().name.clone())
            .collect();
        let type_chain: String = type_chain.join(" -> ");
        let type_chain = format!("Type dependency chain: {}", type_chain);

        result.free_notes = vec![type_chain];
        result
    }

    pub fn function_infinite_dependency_chain(
        source_function: &Function,
        function_chain: Vec<Rc<RefCell<Function>>>,
        location: SourceOrigin,
    ) -> CompilationError {
        let mut result = CompilationError::new(
            "E9044",
            format!(
                "call to function `{}` causes an infinite function dependency chain",
                source_function.name
            ),
            location,
        );

        let function_chain: Vec<_> = function_chain
            .iter()
            .map(|function| function.borrow().name.clone())
            .collect();
        let function_chain: String = function_chain.join("\n -> ");
        let function_chain = format!("Function dependency chain:\n    {}", function_chain);

        result.free_notes = vec![function_chain];
        result
    }

    pub fn logical_operator_operand_wrong_type(
        operator: &str,
        actual_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9045",
            format!(
                "operand of logical operator `{}` must have type `bool`, not `{}`",
                operator, actual_type
            ),
            location,
        )
    }

    pub fn is_expr_operand_wrong_type(
        actual_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9046",
            format!(
                "only pointers can be compared using `is`, but this is a value of type `{}`",
                actual_type
            ),
            location,
        )
    }

    pub fn is_expr_type_mismatch(
        lhs_type: &str,
        rhs_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9047",
            format!(
                "`is` expression operands must have same type, but here they are different: `{}` and `{}`",
                lhs_type,
                rhs_type,
            ),
            location,
        )
    }

    pub fn null_expr_type_cannot_be_inferred(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9048",
            format!("type of `null` expression cannot be inferred from context"),
            location,
        )
    }

    // Next code: E9050.
}

fn explain_expression_type(expression: &Expression, error: &mut CompilationError) {
    fn explain_root_causes(expression: &Expression, error: &mut CompilationError) {
        match expression.kind() {
            ExpressionKind::Block(block) => {
                if !block.value.is_empty() {
                    explain_root_causes(&block.value, error);
                } else if let Some(instruction) = block.instructions.last() {
                    error.bound_notes.push((
                        instruction.location().as_plain(),
                        "block ends with a statement, not an expression, so its type is `void`"
                            .to_string(),
                    ));
                } else {
                    error.bound_notes.push((
                        block.location.as_plain(),
                        "empty block has type `void`, which becomes the type of the overall expression".to_string()));
                }
            }
            // TODO also dig into `if`s.
            _ => error.bound_notes.push((
                expression.location().as_plain(),
                format!(
                    "has type `{}`, which becomes the type of the overall expression",
                    expression.type_().borrow().name
                ),
            )),
        }
    }

    match expression.kind() {
        ExpressionKind::Block(_) => explain_root_causes(expression, error),
        _ => (),
    }
}

impl NamedEntityKind {
    fn text(&self) -> &'static str {
        match self {
            NamedEntityKind::Variable => "variable",
            NamedEntityKind::Function => "function",
            NamedEntityKind::Type => "type",
            NamedEntityKind::TypeTemplate => "type template",
            NamedEntityKind::Field => "field",
            NamedEntityKind::Method => "method",
        }
    }

    fn indefinite_article(&self) -> &'static str {
        use NamedEntityKind::*;
        match self {
            Variable | Function | Type | TypeTemplate | Field | Method => "a",
        }
    }

    fn text_with_indefinite_article(&self) -> String {
        format!("{} {}", self.indefinite_article(), self.text())
    }
}
