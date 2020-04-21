use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Type, TypeRegistry, ValueCategory, Variable};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct VariableExpr {
    pub variable: Rc<RefCell<Variable>>,
    pub location: SourceOrigin,
}

impl ExpressionKindImpl for VariableExpr {
    fn calculate_type(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.variable.borrow().type_)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Lvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
