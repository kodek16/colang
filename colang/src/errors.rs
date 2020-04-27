//! Compilation error definitions.

use codespan_reporting::diagnostic::{Diagnostic, Label};

use crate::ast;
use crate::program::{
    Expression, ExpressionKind, Function, Type, TypeCycleThroughFields, TypeTemplate, Variable,
};
use crate::scope::{GeneralNamedEntity, NamedEntityKind};
use crate::source::{InputSpan, InputSpanFile, SourceOrigin};
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
    fn new(code: &'static str, message: impl Into<String>) -> CompilationError {
        CompilationError {
            code,
            message: message.into(),
            location: None,
            subtitle: None,
            bound_notes: Vec::new(),
            free_notes: Vec::new(),
        }
    }

    fn with_location(self, location: SourceOrigin) -> CompilationError {
        let (location, location_note) = match location {
            SourceOrigin::Plain(span) => (span, None),
            SourceOrigin::AutoDeref(span) => {
                (span, Some("expression was automatically dereferenced"))
            }
            SourceOrigin::DereferencedIndex(span) => (
                span,
                Some("`index` method return value was automatically dereferenced"),
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
            SourceOrigin::MissingReturnValue(span) => (span, Some("return value is not specified")),
        };

        let location_note = location_note.map(|note| (location, note.to_string()));

        let mut bound_notes = self.bound_notes;
        if let Some(location_note) = location_note {
            bound_notes.push(location_note);
        }

        CompilationError {
            code: self.code,
            message: self.message,
            location: Some(location),
            subtitle: self.subtitle,
            bound_notes,
            free_notes: self.free_notes,
        }
    }

    fn with_subtitle(self, subtitle: impl Into<String>) -> CompilationError {
        CompilationError {
            subtitle: Some(subtitle.into()),
            ..self
        }
    }

    fn with_bound_note(
        mut self,
        location: SourceOrigin,
        note: impl Into<String>,
    ) -> CompilationError {
        self.bound_notes.push((location.as_plain(), note.into()));
        self
    }

    fn maybe_with_bound_note<N: Into<String>>(
        self,
        location: Option<SourceOrigin>,
        note: impl FnOnce() -> N,
    ) -> CompilationError {
        if let Some(location) = location {
            self.with_bound_note(location, note())
        } else {
            self
        }
    }

    fn maybe_with_type_explanation(mut self, expression: &Expression) -> CompilationError {
        maybe_explain_expression_type(expression, &mut self);
        self
    }

    fn with_free_note(mut self, note: impl Into<String>) -> CompilationError {
        self.free_notes.push(note.into());
        self
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

        CompilationError::new("E9000", message).with_location(SourceOrigin::Plain(location))
    }

    // Scope errors:

    pub fn named_entity_not_found(
        name: &str,
        kind: NamedEntityKind,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9001",
            format!(
                "no {} with name `{}` could be found in the current scope",
                kind.text(),
                name
            ),
        )
        .with_location(location)
        .with_subtitle(format!("refers to an unknown {}", kind.text()))
    }

    pub fn named_entity_kind_mismatch<G: GeneralNamedEntity>(
        name: &str,
        expected: NamedEntityKind,
        actual: &G,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9002",
            format!(
                "expected `{}` to be {}, but it is {}",
                name,
                actual.kind().text_with_indefinite_article(),
                expected.text_with_indefinite_article(),
            ),
        )
        .with_location(location)
        .with_subtitle(format!("`{}` is used here in a type context", name))
        .maybe_with_bound_note(actual.definition_site(), || {
            format!(
                "`{}` is defined here as {}",
                actual.name(),
                actual.kind().text_with_indefinite_article(),
            )
        })
    }

    pub fn named_entity_already_defined<G: GeneralNamedEntity>(
        name: &str,
        existing: &G,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9003",
            format!(
                "{} with name `{}` is already defined in this scope",
                existing.kind().text_with_indefinite_article(),
                name
            ),
        )
        .with_location(location)
        .with_subtitle(format!("attempted to redefine `{}` here", name))
        .maybe_with_bound_note(existing.definition_site(), || {
            format!("`{}` previously defined here", name)
        })
    }

    // Type is not as expected:

    pub fn condition_is_not_bool(actual_type: &Type, location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9004",
            format!(
                "condition must have type `bool`, not `{}`",
                actual_type.name
            ),
        )
        .with_location(location)
        .with_subtitle(format!(
            "has type `{}`, but only `bool` can be used here",
            actual_type.name
        ))
    }

    pub fn array_size_not_int(size: &Expression) -> CompilationError {
        let size_type = size.type_().borrow();

        CompilationError::new(
            "E9005",
            format!("array size must be of type `int`, not `{}`", size_type.name),
        )
        .with_location(size.location())
        .with_subtitle(format!("size has type `{}`", size_type.name))
        .maybe_with_type_explanation(size)
    }

    pub fn while_body_not_void(body: &Expression) -> CompilationError {
        CompilationError::new(
            "E9006",
            format!(
                "`while` loop body must have type `void`, not `{}`",
                body.type_().borrow().name
            ),
        )
        .with_location(body.location())
        .with_subtitle(format!("body has type `{}`", body.type_().borrow().name))
        .maybe_with_type_explanation(body)
    }

    pub fn can_only_dereference_pointer(expression: &Expression) -> CompilationError {
        let actual_type = expression.type_().borrow();

        CompilationError::new(
            "E9007",
            format!("can only dereference pointers, not `{}`", actual_type.name),
        )
        .with_location(expression.location())
        .with_subtitle(format!("has a non-pointer type `{}`", actual_type.name))
        .maybe_with_type_explanation(expression)
    }

    pub fn read_unsupported_type(target: &Expression) -> CompilationError {
        let actual_type = target.type_().borrow();

        CompilationError::new(
            "E9008",
            format!(
                "can only read `int` and `string` values, not `{}`",
                actual_type.name
            ),
        )
        .with_location(target.location())
        .with_subtitle(format!(
            "target has type `{}` that does not support reading",
            actual_type.name
        ))
        .maybe_with_type_explanation(target)
    }

    pub fn readln_unsupported_type(target: &Expression) -> CompilationError {
        let actual_type = target.type_().borrow();

        CompilationError::new(
            "E9045",
            format!(
                "`readln` can be only used with strings, not `{}`",
                actual_type.name
            ),
        )
        .with_location(target.location())
        .with_subtitle(format!(
            "target has type `{}` that does not support reading by line",
            actual_type.name
        ))
        .maybe_with_type_explanation(target)
    }

    pub fn write_value_is_not_stringable(value: &Expression) -> CompilationError {
        CompilationError::new(
            "E9009",
            format!(
                "can only write values of type `string` or convertible to `string`, not `{}`",
                value.type_().borrow().name
            ),
        )
        .with_location(value.location())
        .with_subtitle(format!("has type `{}`", value.type_().borrow().name))
        .maybe_with_type_explanation(value)
    }

    // Void-sanity:

    pub fn explicit_reference_to_void(location: SourceOrigin) -> CompilationError {
        CompilationError::new("E9010", "cannot explicitly refer to `void` type")
            .with_location(location)
            .with_subtitle("`void` type cannot be used explicitly")
    }

    pub fn variable_no_type_or_initializer(
        variable_name: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9011",
            format!(
                "variable `{}` cannot be defined with no type or initializer expression",
                variable_name
            ),
        )
        .with_location(location)
        .with_subtitle(format!("type of `{}` is unknown here", variable_name))
        .with_bound_note(
            location,
            format!(
                "help: try adding a type, for example `{}: int`",
                variable_name
            ),
        )
    }

    pub fn variable_initializer_is_void(
        variable_name: &str,
        initializer: &Expression,
    ) -> CompilationError {
        CompilationError::new(
            "E9012",
            format!(
                "cannot initialize variable `{}` with a value of type `void`",
                variable_name
            ),
        )
        .with_location(initializer.location())
        .with_subtitle("has type `void`, so cannot be used as an initializer")
        .maybe_with_type_explanation(initializer)
    }

    pub fn return_stmt_with_value_in_void_function(
        function: &Function,
        expression: &Expression,
    ) -> CompilationError {
        CompilationError::new(
            "E9043",
            format!(
                "cannot return a value from function `{}` with no return type",
                function.name
            ),
        )
        .with_location(expression.location())
        .with_subtitle("unexpected value in `return`")
        .with_bound_note(
            function.definition_site.unwrap(),
            format!("function `{}` defined with no return type", function.name),
        )
    }

    pub fn return_stmt_without_value_in_non_void_function(
        function: &Function,
        location: SourceOrigin,
    ) -> CompilationError {
        let return_type = function.return_type.borrow();

        CompilationError::new(
            "E9044",
            format!(
                "`return` must specify a return value of type `{}` in function `{}`",
                return_type.name, function.name
            ),
        )
        .with_location(location)
        .with_subtitle(format!("expected a value of type `{}`", return_type.name))
        .with_bound_note(
            function.definition_site.unwrap(),
            format!(
                "function `{}` defined with return type `{}`",
                function.name, return_type.name
            ),
        )
    }

    // TODO: remove after void-sanity is achieved
    pub fn if_expression_missing_else(then_type: &str, location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9013",
            format!(
                "`if` expression without `else` branch can only be `void`, not `{}`",
                then_type
            ),
        )
        .with_location(location)
    }

    // Type mismatches:

    pub fn assignment_type_mismatch(
        target: &Expression,
        value: &Expression,
        assignment_location: SourceOrigin,
    ) -> CompilationError {
        let target_type = target.type_().borrow();
        let value_type = value.type_().borrow();

        CompilationError::new(
            "E9014",
            format!(
                "cannot assign value of type `{}` to a target of type `{}`",
                value_type.name, target_type.name
            ),
        )
        .with_location(assignment_location)
        .with_subtitle("types must be the same")
        .with_bound_note(
            target.location(),
            format!("target has type `{}`", target_type.name),
        )
        .with_bound_note(
            value.location(),
            format!("value has type `{}`", value_type.name),
        )
        .maybe_with_type_explanation(value)
    }

    pub fn function_body_type_mismatch(function: &Function, body: &Expression) -> CompilationError {
        let return_type = function.return_type.borrow();

        CompilationError::new(
            "E9015",
            format!(
                "function is expected to return type `{}`, but its body has type `{}`",
                return_type.name,
                body.type_().borrow().name,
            ),
        )
        // TODO point at return type exactly, and not at the whole signature.
        .with_location(function.definition_site.unwrap())
        .with_subtitle(format!(
            "signature specifies return type as `{}`",
            return_type.name
        ))
        .maybe_with_type_explanation(body)
    }

    pub fn return_statement_type_mismatch(
        function: &Function,
        expression: &Expression,
    ) -> CompilationError {
        let expression_type = expression.type_().borrow();
        let return_type = function.return_type.borrow();

        CompilationError::new(
            "E9042",
            format!(
                "cannot return a value of type `{}` from a function that returns `{}`",
                expression_type.name, return_type.name
            ),
        )
        .with_location(expression.location())
        .with_subtitle(format!("has type `{}`", expression_type.name))
        .maybe_with_type_explanation(expression)
        .with_bound_note(
            // TODO point at return type exactly, and not at the whole signature.
            function.definition_site.unwrap(),
            format!(
                "function is defined with return type `{}`",
                return_type.name
            ),
        )
    }

    pub fn if_expression_branch_type_mismatch(
        then: &Expression,
        else_: &Expression,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9016",
            format!(
                "`if` expression branches must have same type, but are different: `{}` and `{}`",
                then.type_().borrow().name,
                else_.type_().borrow().name,
            ),
        )
        .with_location(location)
        .with_subtitle("branches have different types")
        .maybe_with_type_explanation(then)
        .maybe_with_type_explanation(else_)
    }

    pub fn is_expr_type_mismatch(
        lhs: &Expression,
        rhs: &Expression,
        location: SourceOrigin,
    ) -> CompilationError {
        let lhs_type = lhs.type_().borrow();
        let rhs_type = rhs.type_().borrow();

        CompilationError::new(
            "E9017",
            format!(
                "operands to `is` must have same type, not different `{}` and `{}`",
                lhs_type.name, rhs_type.name,
            ),
        )
        .with_location(location)
        .with_subtitle("types must be the same")
        .with_bound_note(
            lhs.location(),
            format!("left operand has type `{}`", lhs_type.name),
        )
        .maybe_with_type_explanation(lhs)
        .with_bound_note(
            rhs.location(),
            format!("right operand has type `{}`", rhs_type.name),
        )
        .maybe_with_type_explanation(rhs)
    }

    pub fn call_argument_type_mismatch(
        argument: &Expression,
        parameter: &Variable,
    ) -> CompilationError {
        CompilationError::new(
            "E9018",
            format!(
                "cannot pass a value of type `{}` as an argument for parameter `{}` of type `{}`",
                argument.type_().borrow().name,
                parameter.name,
                parameter.type_.borrow().name
            ),
        )
        .with_location(argument.location())
        .with_subtitle(format!(
            "argument has type `{}`",
            argument.type_().borrow().name
        ))
        .maybe_with_type_explanation(argument)
        .maybe_with_bound_note(parameter.definition_site, || {
            format!(
                "parameter `{}` is defined here with type `{}`",
                parameter.name,
                parameter.type_.borrow().name
            )
        })
    }

    pub fn array_elements_type_mismatch(
        first_element: &Expression,
        wrong_type_element: &Expression,
    ) -> CompilationError {
        let element_type = wrong_type_element.type_().borrow();
        let inferred_type = first_element.type_().borrow();

        CompilationError::new(
            "E9019",
            format!(
                "array element has type `{}`, but it must have the same type `{}` as other elements",
                element_type.name,
                inferred_type.name
            ))
            .with_location(wrong_type_element.location())
            .with_subtitle(format!("element has type `{}`", element_type.name))
            .maybe_with_type_explanation(wrong_type_element)
            .with_bound_note(first_element.location(), format!(
                "array type was inferred from the first element as `{}`",
                inferred_type.name
            ))
            .maybe_with_type_explanation(first_element)
    }

    // Wrong number of arguments:

    pub fn call_wrong_number_of_arguments(
        function: &Function,
        actual_num: usize,
        location: SourceOrigin,
    ) -> CompilationError {
        // TODO use more precise argument and parameter spans.
        CompilationError::new(
            "E9020",
            format!(
                "function `{}` expects {} argument(s), not {} as given",
                function.name,
                function.parameters.len(),
                actual_num
            ),
        )
        .with_location(location)
        .with_subtitle(if actual_num > function.parameters.len() {
            "too many arguments"
        } else {
            "too few arguments"
        })
        .maybe_with_bound_note(function.definition_site, || {
            format!(
                "function `{}` defined with {} parameter(s)",
                function.name,
                function.parameters.len()
            )
        })
    }

    pub fn wrong_number_of_type_template_arguments(
        template: &TypeTemplate,
        actual_num: usize,
        location: SourceOrigin,
    ) -> CompilationError {
        // TODO use more precise argument and parameter spans.
        CompilationError::new(
            "E9021",
            format!(
                "type template `{}` requires {} type arguments, not {} as given",
                template.name,
                template.type_parameters.len(),
                actual_num
            ),
        )
        .with_location(location)
        .with_subtitle(if actual_num > template.type_parameters.len() {
            "too many type arguments"
        } else {
            "too few type arguments"
        })
        .maybe_with_bound_note(template.definition_site, || {
            format!(
                "type template `{}` defined with {} type parameters",
                template.name,
                template.type_parameters.len()
            )
        })
    }

    // Value category issues:

    pub fn assignment_target_not_lvalue(location: SourceOrigin) -> CompilationError {
        CompilationError::new("E9022", "assignment target must be an lvalue")
            .with_location(location)
            .with_subtitle("expression is an rvalue, but only lvalues can be assigned to")
    }

    pub fn read_target_not_lvalue(location: SourceOrigin) -> CompilationError {
        CompilationError::new("E9023", "`read` statement target must be an lvalue")
            .with_location(location)
            .with_subtitle("expression is an rvalue, but only lvalues can be read")
    }

    pub fn address_of_rvalue(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9024",
            "cannot take address of an rvalue, only lvalues can be addressed",
        )
        .with_location(location)
        .with_subtitle("expression is an rvalue, but only lvalues have addresses")
    }

    // Ambiguous expression type issues:

    pub fn cannot_infer_empty_array_type(location: SourceOrigin) -> CompilationError {
        CompilationError::new("E9025", "empty array type cannot be inferred from context")
            .with_location(location)
            .with_subtitle("array type is unknown")
    }

    pub fn cannot_infer_null_pointer_type(location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9026",
            "`null` pointer type cannot be inferred from context",
        )
        .with_location(location)
        .with_subtitle("pointer type is unknown")
    }

    // `self` parameter issues:

    pub fn self_must_be_lvalue(receiver: &Expression, method: &Function) -> CompilationError {
        CompilationError::new(
            "E9027",
            format!("`self` must be an lvalue for method `{}`", method.name),
        )
        .with_location(receiver.location())
        .with_subtitle("`self` is an rvalue here")
        .maybe_with_bound_note(
            method
                .parameters
                .get(0)
                .and_then(|self_| self_.borrow().definition_site),
            || {
                format!(
                    "method `{}` takes `self` by pointer, so it needs to be an lvalue",
                    method.name
                )
            },
        )
    }

    pub fn self_not_in_method_signature(
        function: &Function,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9028",
            format!(
                "`self` can only appear as a method parameter, but `{}` is a function",
                function.name
            ),
        )
        .with_location(location)
        .with_subtitle("`self` not allowed here")
    }

    pub fn self_in_function_body(location: SourceOrigin) -> CompilationError {
        CompilationError::new("E9029", "`self` is not defined outside of methods")
            .with_location(location)
            .with_subtitle("`self` is not defined here")
    }

    pub fn self_is_not_first_parameter(location: SourceOrigin) -> CompilationError {
        CompilationError::new("E9030", "`self` must be the first parameter of a method")
            .with_location(location)
            .with_subtitle("`self` not allowed here")
    }

    // TODO: remove this once associated functions are introduced
    pub fn method_first_parameter_is_not_self(signature: SourceOrigin) -> CompilationError {
        CompilationError::new("E9031", "first parameter for methods must be `self`")
            .with_location(signature)
    }

    // Literals:

    // TODO: test this error once `\xdd` escapes are implemented and non UTF-8 literals are
    // actually possible.
    pub fn literal_not_utf8(location: SourceOrigin) -> CompilationError {
        CompilationError::new("E9032", "literal must be a valid UTF-8 sequence")
            .with_location(location)
            .with_subtitle("string is not valid UTF-8")
    }

    pub fn char_literal_bad_length(actual_len: usize, location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9033",
            format!(
                "`char` literals must contain exactly one UTF-8 byte, not {}",
                actual_len
            ),
        )
        .with_location(location)
        .with_subtitle(format!("literal length is {} bytes in UTF-8", actual_len))
    }

    pub fn unknown_escape_sequence(sequence: &str, location: SourceOrigin) -> CompilationError {
        CompilationError::new(
            "E9034",
            format!("unknown escape sequence: \"{}\"", sequence),
        )
        .with_location(location)
        .with_subtitle(format!(
            "literal contains unknown escape sequence \"{}\"",
            sequence
        ))
    }

    // Operator issues:

    // TODO: remove this when index support is implemented via trait.
    pub fn index_method_returns_not_pointer(
        type_name: &str,
        actual_return_type: &str,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9035",
            format!("`index` method must return a pointer in order to be used in indexing expressions, but for type `{}` it returns `{}`",
                type_name,
                actual_return_type,
            ))
            .with_location(location)
    }

    pub fn binary_operator_unsupported_types(
        operator: &str,
        lhs: &Expression,
        rhs: &Expression,
        location: SourceOrigin,
    ) -> CompilationError {
        let lhs_type = lhs.type_().borrow();
        let rhs_type = rhs.type_().borrow();

        CompilationError::new(
            "E9036",
            format!(
                "operator `{}` cannot be used with types `{}` and `{}`",
                operator, lhs_type.name, rhs_type.name,
            ),
        )
        .with_location(location)
        .with_subtitle(format!(
            "operator `{}` is not defined for `{}` and `{}`",
            operator, lhs_type.name, rhs_type.name
        ))
        .with_bound_note(
            lhs.location(),
            format!("left operand has type `{}`", lhs_type.name),
        )
        .maybe_with_type_explanation(lhs)
        .with_bound_note(
            rhs.location(),
            format!("right operand has type `{}`", rhs_type.name),
        )
        .maybe_with_type_explanation(rhs)
    }

    pub fn logical_operator_operand_wrong_type(
        operator: &str,
        operand: &Expression,
    ) -> CompilationError {
        let actual_type = operand.type_().borrow();

        CompilationError::new(
            "E9037",
            format!(
                "operand of logical operator `{}` must have type `bool`, not `{}`",
                operator, actual_type.name
            ),
        )
        .with_location(operand.location())
        .with_subtitle(format!("operand has type `{}`", actual_type.name))
        .maybe_with_type_explanation(operand)
    }

    pub fn is_expr_operand_wrong_type(operand: &Expression) -> CompilationError {
        let actual_type = operand.type_().borrow();

        CompilationError::new(
            "E9038",
            format!(
                "only pointers can be compared using `is`, but operand has type `{}`",
                actual_type.name
            ),
        )
        .with_location(operand.location())
        .with_subtitle(format!("operand has type `{}`", actual_type.name))
        .maybe_with_type_explanation(operand)
    }

    // Infinite chains:

    pub fn type_infinite_dependency_chain(
        source_type: &Type,
        type_chain: Vec<Rc<RefCell<Type>>>,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9039",
            format!(
                "type `{}` causes an infinite type dependency chain",
                source_type.name
            ),
        )
        .with_location(location)
        .with_subtitle("type reference causes an infinite type chain")
        .with_free_note({
            let type_chain: Vec<_> = type_chain
                .iter()
                .take(8)
                .map(|type_| type_.borrow().name.clone())
                .collect();
            let type_chain: String = type_chain.join("\n -> ");
            format!("Type dependency chain:\n    {}\n -> ...", type_chain)
        })
    }

    pub fn function_infinite_dependency_chain(
        source_function: &Function,
        function_chain: Vec<Rc<RefCell<Function>>>,
        location: SourceOrigin,
    ) -> CompilationError {
        CompilationError::new(
            "E9040",
            format!(
                "call to function `{}` causes an infinite function dependency chain",
                source_function.name
            ),
        )
        .with_location(location)
        .with_subtitle("call causes an infinite function instantiation chain")
        .with_free_note({
            // TODO use qualified method names here
            let function_chain: Vec<_> = function_chain
                .iter()
                .take(8)
                .map(|function| function.borrow().name.clone())
                .collect();
            let function_chain: String = function_chain.join("\n -> ");
            format!(
                "Function dependency chain:\n    {}\n -> ...",
                function_chain
            )
        })
    }

    pub fn type_cycle_through_fields(cycle: TypeCycleThroughFields) -> CompilationError {
        let anchor_type = cycle.anchor_type.borrow();
        let anchor_field = cycle.fields[0].borrow();

        CompilationError::new(
            "E9046",
            format!(
                "type `{}` is a part of a type cycle through fields",
                anchor_type.name,
            ),
        )
        .with_location(anchor_field.definition_site.unwrap())
        .with_subtitle("field type starts a type cycle")
        .with_free_note({
            let type_cycle: Vec<_> = cycle
                .fields
                .iter()
                .map(|field| {
                    let field = field.borrow();
                    format!(
                        "`{}` (through field `{}`)",
                        field.type_.borrow().name,
                        field.name
                    )
                })
                .collect();
            let type_cycle = type_cycle.join("\n -> ");
            format!(
                "Type dependency cycle:\n    `{}`\n -> {}",
                anchor_type.name, type_cycle
            )
        })
    }

    // Missing `main`:

    pub fn main_function_not_found() -> CompilationError {
        CompilationError::new("E9041", "`main` function not found: you must define one")
    }

    // Next code: E9047.
}

fn maybe_explain_expression_type(expression: &Expression, error: &mut CompilationError) {
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
