use crate::program::statements::StatementKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Statement};
use crate::source::SourceOrigin;

/// A statement that reads from stdin and stores the result in an lvalue.
pub struct ReadStmt {
    /// Where to store the read data.
    ///
    /// Must be lvalue of either type `string` or `int`. If the type is `int`, `whole_line` must
    /// be false.
    pub target: Expression,

    /// If this flag is set, the next "clean" line is consumed from stdin completely.
    pub whole_line: bool,

    /// The location of the source code that produced this statement.
    pub location: SourceOrigin,
}

impl StatementKind for ReadStmt {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for ReadStmt {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.target)
    }
}
