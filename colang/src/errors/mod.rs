//! Compilation error definitions.

mod kinds;

use crate::program::{Expression, ExpressionImpl, ExpressionKind, Statement, StatementKind};
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
    fn maybe_with_type_explanation(self, expression: &Expression) -> CompilationError {
        fn explain_root_causes(
            error: CompilationError,
            expression: &Expression,
        ) -> CompilationError {
            match **expression {
                ExpressionImpl::Block(ref block) => {
                    explain_root_causes(error, block.value.as_ref().unwrap())
                }
                // TODO(#8) also dig into `if`s.
                _ => error.with_bound_note(
                    expression.location(),
                    format!(
                        "has type `{}`, which becomes the type of the overall expression",
                        expression.type_().borrow().name
                    ),
                ),
            }
        }

        match **expression {
            ExpressionImpl::Block(_) => explain_root_causes(self, expression),
            _ => self,
        }
    }

    /// Potentially annotates an error with an explanation of why a dual node is a statement.
    ///
    /// For statements that are not ambiguous (i.e. not dual nodes), the explanation is not added.
    fn maybe_with_dual_node_statement_explanation(self, statement: &Statement) -> CompilationError {
        fn explain_root_causes(error: CompilationError, statement: &Statement) -> CompilationError {
            match statement {
                Statement::Block(ref block) => {
                    if let Some(statement) = block.statements.last() {
                        explain_root_causes(error, statement)
                    } else {
                        error.with_bound_note(block.location, "an empty block is a statement")
                    }
                },

                Statement::If(ref if_) => {
                    match if_.else_ {
                        Some(ref _else_) => {
                            // TODO(#8): check both branches and make sure that the error messages
                            // make sense. E.g. one of the branches might end with an Eval statement,
                            // which is an expression - need to make sure that the error message is
                            // not confusing in that case.
                            explain_root_causes(error, &if_.then)
                        },
                        None => {
                            error.with_bound_note(if_.location, "an `if` without an `else` branch is always a statement")
                        }
                    }
                },

                Statement::Call(ref call) => {
                    error.with_bound_note(
                        call.location,
                        format!(
                            "a call to void function `{}` is a statement, so the enclosing block is also a statement",
                            call.function.borrow().name
                        )
                    )
                },

                _ => error.with_bound_note(
                    statement.location(),
                    "is a statement, so the enclosing block is also a statement",
                ),
            }
        }

        match statement {
            Statement::Block(_) | Statement::If(_) => explain_root_causes(self, statement),
            Statement::Call(call) => self.with_bound_note(
                call.location,
                format!(
                    "a call to void function `{}` is a statement, not an expression",
                    call.function.borrow().name
                ),
            ),
            _ => self,
        }
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
