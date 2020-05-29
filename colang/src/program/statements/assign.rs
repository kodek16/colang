use crate::program::statements::StatementKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Statement};
use crate::source::SourceOrigin;

/// A statement that updates an lvalue setting it to a new value.
pub struct AssignStmt {
    /// The lvalue to be updated.
    ///
    /// Must be an lvalue.
    pub target: Expression,

    /// The new value to be set.
    ///
    /// Must have the same type as `target`.
    pub value: Expression,

    /// The location of source code that produced this statement.
    pub location: SourceOrigin,
}

impl StatementKind for AssignStmt {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for AssignStmt {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::vec::IntoIter<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        vec![&mut self.target, &mut self.value].into_iter()
    }
}
