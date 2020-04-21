use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct IfExpr {
    pub cond: Box<Expression>,
    pub then: Box<Expression>,
    pub else_: Box<Expression>,
    pub location: SourceOrigin,
}

impl ExpressionKindImpl for IfExpr {
    fn calculate_type(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(self.then.type_())
    }

    fn calculate_value_category(&self) -> ValueCategory {
        match (self.then.value_category(), self.else_.value_category()) {
            (ValueCategory::Lvalue, ValueCategory::Lvalue) => ValueCategory::Lvalue,
            _ => ValueCategory::Rvalue,
        }
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
