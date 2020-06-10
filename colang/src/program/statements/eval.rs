use crate::program::expressions::{Expression, ExpressionKind};
use crate::program::statements::StatementKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::Statement;
use crate::source::SourceOrigin;

/// A statement that evaluates an expression and does not use its value.
pub struct EvalStmt {
    /// Expression to be evaluated.
    pub expression: Expression,
}

impl StatementKind for EvalStmt {
    fn location(&self) -> SourceOrigin {
        self.expression.location()
    }
}

impl<'a> LocalCodeNode<'a> for EvalStmt {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.expression)
    }
}
