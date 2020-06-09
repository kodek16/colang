use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Statement, StatementKind};
use crate::source::SourceOrigin;

/// A statement that executes one of its branches depending on the value of the condition.
///
/// This class represents "if-statements". "if-expressions", which is a related code node,
/// is represented by `expressions::if_::IfExpr`.
pub struct IfStmt {
    /// The condition expression that determines which branch is executed.
    ///
    /// Must have type `bool`.
    pub cond: Box<Expression>,

    /// The branch that gets executed when `condition` evaluates to `true`.
    pub then: Box<Statement>,

    /// The branch that gets executed when `condition` evaluates to `false`.
    ///
    /// If absent, the branch should be considered empty.
    pub else_: Option<Box<Statement>>,

    /// The location of source code that produced this statement.
    pub location: SourceOrigin,
}

impl StatementKind for IfStmt {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for IfStmt {
    type StmtIter = std::vec::IntoIter<&'a mut Statement>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        match &mut self.else_ {
            Some(else_) => vec![&mut *self.then, &mut *else_].into_iter(),
            None => vec![&mut *self.then].into_iter(),
        }
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.cond)
    }
}
