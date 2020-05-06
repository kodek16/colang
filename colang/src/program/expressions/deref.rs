use crate::program::expressions::ExpressionKind;
use crate::program::{Expression, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct DerefExpr {
    pub pointer: Box<Expression>,
    pub location: SourceOrigin,
}

impl ExpressionKind for DerefExpr {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        if let Some(target_type) = self.pointer.type_().borrow().pointer_target_type() {
            target_type
        } else {
            panic!(
                "DerefExpr is in an invalid state: pointer expression type is `{}`",
                self.pointer.type_().borrow().name
            )
        }
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Lvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
