use crate::ast::InputSpan;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub struct NewExpr {
    target_type: Rc<RefCell<Type>>,
}

impl NewExpr {
    pub fn new(
        target_type: Rc<RefCell<Type>>,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        let type_ = types.pointer_to(&target_type);
        let kind = ExpressionKind::New(NewExpr { target_type });

        Expression {
            kind,
            type_,
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        }
    }

    pub fn target_type(&self) -> &Rc<RefCell<Type>> {
        &self.target_type
    }
}
