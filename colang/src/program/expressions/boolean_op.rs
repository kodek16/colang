use crate::program::expressions::ExpressionKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Instruction, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that performs a logical operation that may shortcircuit.
pub struct BooleanOpExpr {
    /// The logical operation to be performed.
    pub op: BooleanOp,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

/// A logical operation performed by `BooleanOpExpr`.
///
/// Some of the operations support shortcircuiting behavior.
pub enum BooleanOp {
    /// Logical AND. If the first operand is `false`, the second does not get evaluated.
    And(Box<Expression>, Box<Expression>),

    /// Logical OR. If the first operand is `true`, the second does not get evaluated.
    Or(Box<Expression>, Box<Expression>),

    /// Logical NOT.
    Not(Box<Expression>),
}

impl ExpressionKind for BooleanOpExpr {
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(types.bool())
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for BooleanOpExpr {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::vec::IntoIter<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        match &mut self.op {
            BooleanOp::And(lhs, rhs) => vec![&mut **lhs, &mut **rhs].into_iter(),
            BooleanOp::Or(lhs, rhs) => vec![&mut **lhs, &mut **rhs].into_iter(),
            BooleanOp::Not(operand) => vec![&mut **operand].into_iter(),
        }
    }
}
