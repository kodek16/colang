use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, SourceOrigin, Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub struct AddressExpr {
    pub target: Box<Expression>,
    pub location: SourceOrigin,
}

impl ExpressionKindImpl for AddressExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.pointer_to(&self.target.type_())
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
