use crate::program::statements::StatementKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Statement};
use crate::source::SourceOrigin;

/// A statement that writes a string to stdout.
pub struct WriteStmt {
    /// The string to be written.
    ///
    /// Must have type `string`.
    pub expression: Expression,

    /// The location in the source code that produced this statement.
    pub location: SourceOrigin,
}

impl StatementKind for WriteStmt {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for WriteStmt {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.expression)
    }
}
