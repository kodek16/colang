use crate::ast::InputSpan;
use crate::program::{Expression, ExpressionKind, Variable};
use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub struct FieldAccessExpr {
    receiver: Box<Expression>,
    field: Rc<RefCell<Variable>>,
}

impl FieldAccessExpr {
    pub fn new(receiver: Expression, field: Rc<RefCell<Variable>>, span: InputSpan) -> Expression {
        assert!(receiver.type_.borrow().fields().any(|f| *f == field));

        let type_ = Rc::clone(&field.borrow().type_);
        let value_category = receiver.value_category;
        let kind = FieldAccessExpr {
            receiver: Box::new(receiver),
            field,
        };

        Expression {
            kind: ExpressionKind::FieldAccess(kind),
            type_,
            value_category,
            span: Some(span),
        }
    }

    pub fn receiver(&self) -> &Expression {
        &self.receiver
    }

    pub fn field(&self) -> impl Deref<Target = Variable> + '_ {
        self.field.borrow()
    }
}
