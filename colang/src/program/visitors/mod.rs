//! A framework for different kinds of function body code processors and rewriters.

pub mod valid;

mod node;
mod visitor;

pub use node::LocalCodeNode;
pub use visitor::LocalVisitor;
