use crate::ast::InputSpan;
use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory, Variable};
use std::cell::RefCell;
use std::rc::Rc;

pub struct FieldAccessExpr {
    pub receiver: Box<Expression>,
    pub field: Rc<RefCell<Variable>>,
}

impl FieldAccessExpr {
    pub fn new(
        receiver: Expression,
        field: Rc<RefCell<Variable>>,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        assert!(receiver.type_.borrow().fields().any(|f| *f == field));

        let kind = ExpressionKind::FieldAccess(FieldAccessExpr {
            receiver: Box::new(receiver),
            field,
        });

        Expression::new(kind, Some(span), types)
    }
}

impl ExpressionKindImpl for FieldAccessExpr {
    fn calculate_type(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.field.borrow().type_)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        self.receiver.value_category()
    }
}
