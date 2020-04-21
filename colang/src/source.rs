#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub struct InputSpan {
    pub file: InputSpanFile,
    pub start: usize,
    pub end: usize,
}

#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum InputSpanFile {
    UserProgram,
    Std,
}

impl InputSpan {
    /// Returns a span for the first character of the file.
    /// Can be useful as a placeholder, when the caller is sure that the span is not going
    /// to be displayed to the end user.
    pub fn top_of_file() -> InputSpan {
        InputSpan {
            file: InputSpanFile::UserProgram,
            start: 0,
            end: 1,
        }
    }
}

/// Represents the original location in source code of some target language expression or statement.
/// If an expression was synthesised (e.g. through auto-deref), this information is also contained
/// in its `SourceOrigin`.
#[derive(Copy, Clone, Debug)]
pub enum SourceOrigin {
    /// Object is directly backed by source code.
    Plain(InputSpan),

    /// Object is a `DerefExpr` produced through automatic dereferencing.
    AutoDeref(InputSpan),

    /// Object is a `DerefExpr` produced after calling `index` method from an indexing expression.
    DereferencedIndex(InputSpan),

    /// Object is an `AddressExpr` produced while processing a `read` statement.
    AddressedForRead(InputSpan),

    /// Object is a `CallExpr` calling an internal function backing a `read` statement.
    ReadFunctionCall(InputSpan),

    /// Object is a `CallExpr` calling an internal conversion function to `string` in contexts
    /// where implicit conversions are allowed.
    Stringified(InputSpan),

    /// Object is an `AddressExpr` wrapping a receiver of a method accepting `&self`.
    AddressedForMethodCall(InputSpan),

    /// Object is an empty expression created as a value of a missing `else` branch.
    /// Enclosing `if` expression location is preserved.
    MissingElse(InputSpan),

    /// Object is an empty expression created as a value of a block not terminated with an
    /// expression.
    MissingBlockValue(InputSpan),
}

impl SourceOrigin {
    pub fn as_plain(&self) -> InputSpan {
        match self {
            SourceOrigin::Plain(span) => *span,
            SourceOrigin::AutoDeref(span) => *span,
            SourceOrigin::DereferencedIndex(span) => *span,
            SourceOrigin::AddressedForRead(span) => *span,
            SourceOrigin::ReadFunctionCall(span) => *span,
            SourceOrigin::Stringified(span) => *span,
            SourceOrigin::AddressedForMethodCall(span) => *span,
            SourceOrigin::MissingElse(span) => *span,
            SourceOrigin::MissingBlockValue(span) => *span,
        }
    }
}
