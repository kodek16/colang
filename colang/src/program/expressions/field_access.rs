use crate::program::expressions::ExpressionKind;
use crate::program::{Expression, Field, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct FieldAccessExpr {
    pub receiver: Box<Expression>,
    pub field: Rc<RefCell<Field>>,
    pub location: SourceOrigin,
}

impl ExpressionKind for FieldAccessExpr {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.field.borrow().type_)
    }

    fn value_category(&self) -> ValueCategory {
        self.receiver.value_category()
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
