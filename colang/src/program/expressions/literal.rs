use crate::program::expressions::ExpressionKind;
use crate::program::visitors::node::LocalCodeNode;
use crate::program::{Expression, Instruction, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that evaluates to a constant defined directly in the program.
pub struct LiteralExpr {
    /// The value of the constant.
    pub value: LiteralValue,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

/// A constant value contained in `LiteralExpr` and defined directly in the program.
#[derive(Clone)]
pub enum LiteralValue {
    /// A constant value of type `int`.
    Int(i32),

    /// A constant value of type `bool`.
    Bool(bool),

    /// A constant value of type `char`.
    Char(u8),

    /// A constant value of type `string`.
    String(String),
}

impl ExpressionKind for LiteralExpr {
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(match self.value {
            LiteralValue::Int(_) => types.int(),
            LiteralValue::Bool(_) => types.bool(),
            LiteralValue::Char(_) => types.char(),
            LiteralValue::String(_) => types.string(),
        })
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for LiteralExpr {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::iter::Empty<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::empty()
    }
}
