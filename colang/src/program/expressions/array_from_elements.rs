use crate::program::expressions::ExpressionKind;
use crate::program::{Expression, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct ArrayFromElementsExpr {
    pub elements: Vec<Expression>,
    pub location: SourceOrigin,

    // Used for inferring the type of empty arrays.
    pub element_type: Rc<RefCell<Type>>,
}

impl ExpressionKind for ArrayFromElementsExpr {
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.array_of(&self.element_type)
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
