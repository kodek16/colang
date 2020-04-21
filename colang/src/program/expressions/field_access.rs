use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, Type, TypeRegistry, ValueCategory, Variable};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct FieldAccessExpr {
    pub receiver: Box<Expression>,
    pub field: Rc<RefCell<Variable>>,
    pub location: SourceOrigin,
}

impl ExpressionKindImpl for FieldAccessExpr {
    fn calculate_type(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.field.borrow().type_)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        self.receiver.value_category()
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
