//! Compilation error definitions.

mod kinds;

use crate::program::{Expression, ExpressionImpl, ExpressionKind, StatementKind};
use crate::scope::NamedEntityKind;
use crate::source::{InputSpan, InputSpanFile, SourceOrigin};
use codespan_reporting::diagnostic::{Diagnostic, Label};

/// A user-caused error detected during compilation.
///
/// Compilation errors are reported to the user and cause compilation to fail.
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

pub use kinds::constructors::*;

impl CompilationError {
    /// Creates a new compilation error.
    ///
    /// Constructor functions from `kinds` should be used from the outside of this module.
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

    /// Annotates an error with a location in source code which refers to the primary cause.
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
            bound_notes,
            location: Some(location),
            ..self
        }
    }

    /// Annotates an error with a subtitle to be displayed next to the primary cause location.
    fn with_subtitle(self, subtitle: impl Into<String>) -> CompilationError {
        CompilationError {
            subtitle: Some(subtitle.into()),
            ..self
        }
    }

    /// Annotates an error with a note referring to an auxiliary location in code.
    fn with_bound_note(
        mut self,
        location: SourceOrigin,
        note: impl Into<String>,
    ) -> CompilationError {
        self.bound_notes.push((location.as_plain(), note.into()));
        self
    }

    /// Annotates an error with a note only when `location` is present.
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

    /// Annotates an error with an explanation of type deduction for a given expression.
    ///
    /// If the expression is trivial, explanation is not added.
    fn maybe_with_type_explanation(mut self, expression: &Expression) -> CompilationError {
        maybe_explain_expression_type(expression, &mut self);
        self
    }

    /// Annotates an error with a note that cannot be linked to source code in a meaningful way.
    fn with_free_note(mut self, note: impl Into<String>) -> CompilationError {
        self.free_notes.push(note.into());
        self
    }

    /// Builds a `codespan_reporting` diagnostic that can be used for displaying the error.
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
}

fn maybe_explain_expression_type(expression: &Expression, error: &mut CompilationError) {
    fn explain_root_causes(expression: &Expression, error: &mut CompilationError) {
        match **expression {
            ExpressionImpl::Block(ref block) => {
                if let Some(value) = &block.value {
                    explain_root_causes(value, error);
                } else if let Some(statement) = block.statements.last() {
                    error.bound_notes.push((
                        statement.location().as_plain(),
                        "block ends with a statement, not an expression, so its type is `void`"
                            .to_string(),
                    ));
                } else {
                    error.bound_notes.push((
                        block.location.as_plain(),
                        "empty block has type `void`, which becomes the type of the overall expression".to_string()));
                }
            }
            // TODO(#8) also dig into `if`s.
            _ => error.bound_notes.push((
                expression.location().as_plain(),
                format!(
                    "has type `{}`, which becomes the type of the overall expression",
                    expression.type_().borrow().name
                ),
            )),
        }
    }

    match **expression {
        ExpressionImpl::Block(_) => explain_root_causes(expression, error),
        _ => (),
    }
}

impl NamedEntityKind {
    fn text(&self) -> &'static str {
        match self {
            NamedEntityKind::Variable => "variable",
            NamedEntityKind::Function => "function",
            NamedEntityKind::Trait => "trait",
            NamedEntityKind::Type => "type",
            NamedEntityKind::TypeTemplate => "type template",
            NamedEntityKind::Field => "field",
            NamedEntityKind::Method => "method",
        }
    }

    fn indefinite_article(&self) -> &'static str {
        use NamedEntityKind::*;
        match self {
            Variable | Function | Trait | Type | TypeTemplate | Field | Method => "a",
        }
    }

    fn text_with_indefinite_article(&self) -> String {
        format!("{} {}", self.indefinite_article(), self.text())
    }
}
