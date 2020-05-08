//! Provides a tree abstraction over expressions and instructions.

use crate::program::expressions::Expression;
use crate::program::instructions::Instruction;

/// A trait for any kind of statement or expression that provides a tree-like view.
///
/// This trait provides a view of the function body code in tree form, where nodes are either
/// statements or expressions. It is implemented for all kinds of statements and expressions.
pub trait LocalCodeNode<'a>
where
    Self::InstrIter: Iterator<Item = &'a mut Instruction>,
    Self::ExprIter: Iterator<Item = &'a mut Expression>,
{
    type InstrIter;
    type ExprIter;

    fn child_instructions(&'a mut self) -> Self::InstrIter;
    fn child_expressions(&'a mut self) -> Self::ExprIter;
}
