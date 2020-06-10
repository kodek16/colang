use crate::errors::CompilationError;
use crate::program::{Statement, StatementKind};

pub fn statement_used_as_expression(statement: &Statement) -> CompilationError {
    CompilationError::new("E0057", "statement cannot be used as expression")
        .with_location(statement.location())
        .with_subtitle("this is a statement, but an expression was expected")
        .maybe_with_dual_node_statement_explanation(statement)
}
