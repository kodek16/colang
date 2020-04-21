use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct NewExpr {
    pub target_type: Rc<RefCell<Type>>,
    pub location: SourceOrigin,
}

impl ExpressionKindImpl for NewExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.pointer_to(&self.target_type)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
