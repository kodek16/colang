use crate::program::expressions::ExpressionKind;
use crate::program::{Type, TypeRegistry, ValueCategory};
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
