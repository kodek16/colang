use crate::program::statements::StatementKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Statement};
use crate::source::SourceOrigin;

/// A statement that immediately stops the execution of the current function.
///
/// The function return value is set to `expression`.
pub struct ReturnStmt {
    /// The return value for the current function.
    ///
    /// Must have the same type as the function return type.
    pub expression: Expression,

    /// The location of the source code that produced this statement.
    pub location: SourceOrigin,
}

impl StatementKind for ReturnStmt {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for ReturnStmt {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.expression)
    }
}
