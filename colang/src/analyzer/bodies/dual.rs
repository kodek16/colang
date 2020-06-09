//! Dual code node kinds can be either expressions or statements depending on their configuration.

use crate::program::{Call, EvalStmt, Expression, Statement, TypeRegistry};

/// Either an expression or a statement.
pub enum DualNode {
    Expression(Expression),
    Statement(Statement),
}

impl DualNode {
    /// Creates a proper `DualNode` from a function call object.
    pub fn from_call(call: Call, types: &mut TypeRegistry) -> DualNode {
        if call.function.borrow().return_type.borrow().is_void() {
            DualNode::Statement(Statement::Call(call))
        } else {
            DualNode::Expression(Expression::new(call, types))
        }
    }

    /// Converts a node that can potentially be an expression into statement.
    pub fn into_statement(self) -> Statement {
        match self {
            DualNode::Expression(expression) => Statement::Eval(EvalStmt { expression }),
            DualNode::Statement(statement) => statement,
        }
    }
}
