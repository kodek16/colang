use crate::program::expressions::ExpressionKind;
use crate::program::{Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that creates a new value on the heap and evaluates to a pointer to it.
pub struct NewExpr {
    /// The type of the value to be created on the heap.
    pub target_type: Rc<RefCell<Type>>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for NewExpr {
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
