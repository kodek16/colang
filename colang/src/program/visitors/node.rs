//! Provides a tree abstraction over expressions and instructions.

use crate::program::expressions::Expression;
use crate::program::instructions::Instruction;

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
