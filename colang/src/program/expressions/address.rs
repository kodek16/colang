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
    ) -> Result<Expression, CompilationError> {
        if target.value_category != ValueCategory::Lvalue {
            let error = CompilationError::address_of_rvalue(
                target
                    .span
                    .expect("attempt to take address of generated rvalue expression."),
            );
            return Err(error);
        }

        let span = target.span;
        let type_ = types.pointer_to(&target.type_);
        let kind = ExpressionKind::Address(AddressExpr {
            target: Box::new(target),
        });

        let expression = Expression {
            kind,
            type_,
            value_category: ValueCategory::Rvalue,
            span,
        };
        Ok(expression)
    }

    pub fn target(&self) -> &Expression {
        &self.target
    }
}
