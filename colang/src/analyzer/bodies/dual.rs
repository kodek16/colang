//! Dual code node kinds can be either expressions or statements depending on their configuration.

use crate::program::{EvalStmt, Expression, Statement};

/// Either an expression or a statement.
pub enum DualNode {
    Expression(Expression),
    Statement(Statement),
}

impl DualNode {
    /// Converts a node that can potentially be an expression into statement.
    pub fn into_statement(self) -> Statement {
        match self {
            DualNode::Expression(expression) => Statement::Eval(EvalStmt { expression }),
            DualNode::Statement(statement) => statement,
        }
    }
}
