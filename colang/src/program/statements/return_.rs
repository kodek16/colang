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
    /// If the current function is void, this must be `None`. If the current function is non-void,
    /// this must have the same type at the function return type.
    pub expression: Option<Expression>,

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
    type ExprIter = std::option::IterMut<'a, Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        self.expression.iter_mut()
    }
}
