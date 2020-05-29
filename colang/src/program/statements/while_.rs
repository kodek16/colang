use crate::program::statements::StatementKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Statement};
use crate::source::SourceOrigin;

/// A statement that runs an expression and a statement in a loop until expression becomes false.
pub struct WhileStmt {
    /// The loop breaking condition.
    ///
    /// Must be of type `bool`.
    pub cond: Expression,

    /// The body of the loop.
    pub body: Box<Statement>,

    /// The location of the source code that produced this statement.
    pub location: SourceOrigin,
}

impl StatementKind for WhileStmt {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for WhileStmt {
    type StmtIter = std::iter::Once<&'a mut Statement>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::once(&mut self.body)
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.cond)
    }
}
