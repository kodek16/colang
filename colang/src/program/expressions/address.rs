use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, TypeRegistry, ValueCategory};

#[derive(Debug)]
pub struct AddressExpr {
    target: Box<Expression>,
}

impl AddressExpr {
    pub(crate) fn new(
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

        let type_ = types.pointer_to(&target.type_.borrow());
        let kind = ExpressionKind::Address(AddressExpr {
            target: Box::new(target),
        });

        let expression = Expression {
            kind,
            type_,
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        };
        Ok(expression)
    }

    pub(crate) fn new_synthetic(
        target: Expression,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        let target = Box::new(target);
        let type_ = types.pointer_to(&target.type_.borrow());

        Expression {
            kind: ExpressionKind::Address(AddressExpr { target }),
            type_,
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        }
    }

    pub fn target(&self) -> &Expression {
        &self.target
    }
}
