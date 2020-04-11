use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub struct AddressExpr {
    pub target: Box<Expression>,
}

impl AddressExpr {
    pub fn new(
        target: Expression,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        if target.value_category != ValueCategory::Lvalue {
            let error = CompilationError::address_of_rvalue(
                target
                    .span
                    .expect("attempt to take address of generated rvalue expression."),
            );
            return Err(error);
        }

        let kind = ExpressionKind::Address(AddressExpr {
            target: Box::new(target),
        });

        Ok(Expression::new(kind, Some(span), types))
    }

    pub fn new_synthetic(
        target: Expression,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        let kind = ExpressionKind::Address(AddressExpr {
            target: Box::new(target),
        });

        Expression::new(kind, Some(span), types)
    }
}

impl ExpressionKindImpl for AddressExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.pointer_to(&self.target.type_())
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }
}
