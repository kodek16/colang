use crate::ast::InputSpan;
use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub struct NewExpr {
    pub target_type: Rc<RefCell<Type>>,
}

impl NewExpr {
    pub fn new(
        target_type: Rc<RefCell<Type>>,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        let kind = ExpressionKind::New(NewExpr { target_type });
        Expression::new(kind, Some(span), types)
    }
}

impl ExpressionKindImpl for NewExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.pointer_to(&self.target_type)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }
}
