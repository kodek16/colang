//! Dual code node kinds can be either expressions or statements depending on their configuration.

use crate::program::{Expression, Statement};

/// Either an expression or a statement.
pub enum DualNode {
    Expression(Expression),
    Statement(Statement),
}
