use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Statement, StatementKind};
use crate::source::SourceOrigin;

/// A no-op statement produced from a semicolon.
///
/// Refer to the docs in `ast` for more context. From a semantic perspective, this is just a
/// no-op.
pub struct SemicolonStmt {
    /// The location of the semicolon that produced this statement.
    pub location: SourceOrigin,
}

impl StatementKind for SemicolonStmt {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for SemicolonStmt {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::iter::Empty<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::empty()
    }
}
