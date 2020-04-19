use crate::program::{Expression, SourceOrigin, Type, TypeRegistry, ValueCategory};

use crate::program::expressions::ExpressionKindImpl;
use std::cell::RefCell;
use std::rc::Rc;

pub struct ArrayFromElementsExpr {
    pub elements: Vec<Expression>,
    pub location: SourceOrigin,

    // Used for inferring the type of empty arrays.
    pub element_type: Rc<RefCell<Type>>,
}

impl ExpressionKindImpl for ArrayFromElementsExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.array_of(&self.element_type)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
