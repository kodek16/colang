use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct IsExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub location: SourceOrigin,
}

impl ExpressionKindImpl for IsExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(types.bool())
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
