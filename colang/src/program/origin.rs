use crate::ast::InputSpan;

/// Represents the original location in source code of some target language expression or statement.
/// If an expression was synthesised (e.g. through auto-deref), this information is also contained
/// in its `SourceOrigin`.
#[derive(Copy, Clone)]
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

    /// Object is an `EmptyExpr` created as a value of a missing `else` branch.
    /// Enclosing `if` expression location is preserved.
    MissingElse(InputSpan),
}

impl SourceOrigin {
    pub fn as_plain(&self) -> InputSpan {
        match self {
            SourceOrigin::Plain(span) => *span,
            _ => panic!("Attempted to treat a synthetic expression or statement as plain"),
        }
    }
}
