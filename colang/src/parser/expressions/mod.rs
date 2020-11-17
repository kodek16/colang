//! Parser definitions for expression-like nodes.
//!
//! Note that not all nodes that live in this module are actually guaranteed to be expressions
//! after analysis, see `crate::parser::stmt_or_expr`.

pub mod binary_op;
pub mod block;
pub mod if_;

mod bool_literal;
mod int_literal;
mod primary;
mod string_literal;
mod variable;
