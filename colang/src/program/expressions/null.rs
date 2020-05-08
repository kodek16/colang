use crate::program::expressions::ExpressionKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Instruction, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that evaluates to a null pointer.
pub struct NullExpr {
    /// The type that the `null` value supposedly points to.
    pub target_type: Rc<RefCell<Type>>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for NullExpr {
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.pointer_to(&self.target_type)
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for NullExpr {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::iter::Empty<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::empty()
    }
}
