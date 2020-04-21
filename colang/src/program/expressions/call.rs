use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, Function, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct CallExpr {
    pub function: Rc<RefCell<Function>>,
    pub arguments: Vec<Expression>,
    pub location: SourceOrigin,
}

impl ExpressionKindImpl for CallExpr {
    fn calculate_type(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.function.borrow().return_type)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
