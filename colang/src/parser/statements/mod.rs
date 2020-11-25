//! Parser definitions for statement nodes.
//!
//! Note that some nodes that might turn out to be statements after analysis are considered
//! "expression-like", and handled by the `expressions` module.

pub mod semicolon;
pub mod var_decl;
pub mod write;
