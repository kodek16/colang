use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, TypeRegistry, ValueCategory};

#[derive(Debug)]
pub struct DerefExpr {
    pointer: Box<Expression>,
}

impl DerefExpr {
    pub(crate) fn new(
        pointer: Expression,
        types: &TypeRegistry,
    ) -> Result<Expression, CompilationError> {
        let target_type = pointer.type_.borrow().pointer_target_type(types);

        let target_type = match target_type {
            Some(target_type) => target_type,
            None => {
                let error = CompilationError::can_only_dereference_pointer(
                    pointer.type_.borrow().name(),
                    pointer
                        .span
                        .expect("Attempt to dereference generated non-pointer expression"),
                );
                return Err(error);
            }
        };

        let span = pointer.span;
        let kind = ExpressionKind::Deref(DerefExpr {
            pointer: Box::new(pointer),
        });

        let expression = Expression {
            kind,
            type_: target_type,
            value_category: ValueCategory::Lvalue,
            span,
        };
        Ok(expression)
    }

    pub fn pointer(&self) -> &Expression {
        &self.pointer
    }
}
