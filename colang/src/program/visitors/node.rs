//! Provides a tree abstraction over expressions and statements.

use crate::program::expressions::Expression;
use crate::program::statements::Statement;

/// A trait for any kind of statement or expression that provides a tree-like view.
///
/// This trait provides a view of the function body code in tree form, where nodes are either
/// statements or expressions. It is implemented for all kinds of statements and expressions.
pub trait LocalCodeNode<'a>
where
    Self::StmtIter: Iterator<Item = &'a mut Statement>,
    Self::ExprIter: Iterator<Item = &'a mut Expression>,
{
    type StmtIter;
    type ExprIter;

    fn child_statements(&'a mut self) -> Self::StmtIter;
    fn child_expressions(&'a mut self) -> Self::ExprIter;
}
