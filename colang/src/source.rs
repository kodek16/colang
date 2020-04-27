//! This module defines various types that bind generated program IR with the actual source code.

/// Represents a substring (interval) of a file with source code.
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub struct InputSpan {
    /// Source file identifier.
    pub file: InputSpanFile,

    /// First byte included in the interval.
    pub start: usize,

    /// First byte excluded from the interval. That is, the last byte of the interval is `end - 1`.
    pub end: usize,
}

/// An identifier for source files used during compilation.
///
/// Currently, we assume a model where only one file with the actual program is provided by the
/// end user. All other files are provided by the compiler (they may be not actually present
/// on the file system).
#[derive(PartialEq, Eq, Hash, Copy, Clone, Debug)]
pub enum InputSpanFile {
    /// Program source file provided by the end user.
    UserProgram,

    /// Standard library source. See `colang::stdlib`.
    Std,
}

impl InputSpan {
    /// Generates an `InputSpan` referring to the first byte of the user program.
    ///
    /// Can be used as a placeholder when the caller is sure that the span is not going
    /// to be displayed as a part of an error. Usages should be brought to minimum.
    pub fn top_of_file() -> InputSpan {
        InputSpan {
            file: InputSpanFile::UserProgram,
            start: 0,
            end: 1,
        }
    }
}

/// Represents the original location in source code of some target language expression or statement.
///
/// If an expression was synthesised (e.g. through auto-deref), this information is also contained
/// in its `SourceOrigin` so that it can be included in potential error messages.
#[derive(Copy, Clone, Debug)]
pub enum SourceOrigin {
    /// Object is directly backed by source code.
    Plain(InputSpan),

    /// Object is a `DerefExpr` produced through automatic dereferencing.
    AutoDeref(InputSpan),

    /// Object is a `DerefExpr` produced after calling `index` method from an indexing expression.
    DereferencedIndex(InputSpan),

    /// Object is a `CallExpr` calling an internal conversion function to `string` in contexts
    /// where implicit conversions are allowed.
    Stringified(InputSpan),

    /// Object is an `AddressExpr` wrapping a receiver of a method accepting `&self`.
    AddressedForMethodCall(InputSpan),

    /// Object is an empty expression created as a value of a missing `else` branch.
    /// Enclosing `if` expression location is preserved.
    MissingElse(InputSpan),

    /// Object is an empty expression created as a value of a block not terminated with an
    /// expression. The block location is preserved.
    MissingBlockValue(InputSpan),

    /// Object is an empty expression created as a value of a `void` return statement.
    /// The statement location is preserved.
    MissingReturnValue(InputSpan),
}

impl SourceOrigin {
    /// Extracts the underlying `InputSpan` from a `SourceOrigin`.
    pub fn as_plain(&self) -> InputSpan {
        match self {
            SourceOrigin::Plain(span) => *span,
            SourceOrigin::AutoDeref(span) => *span,
            SourceOrigin::DereferencedIndex(span) => *span,
            SourceOrigin::Stringified(span) => *span,
            SourceOrigin::AddressedForMethodCall(span) => *span,
            SourceOrigin::MissingElse(span) => *span,
            SourceOrigin::MissingBlockValue(span) => *span,
            SourceOrigin::MissingReturnValue(span) => *span,
        }
    }
}
