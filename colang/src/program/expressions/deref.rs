use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub struct DerefExpr {
    pub pointer: Box<Expression>,
    pub span: Option<InputSpan>,
}

impl DerefExpr {
    pub fn new(
        pointer: Expression,
        types: &mut TypeRegistry,
        span: Option<InputSpan>,
    ) -> Result<Expression, CompilationError> {
        if !pointer.type_.borrow().is_pointer() {
            let error = CompilationError::can_only_dereference_pointer(
                &pointer.type_.borrow().name,
                pointer
                    .span()
                    .expect("Attempt to dereference generated non-pointer expression"),
            );
            return Err(error);
        }

        let kind = ExpressionKind::Deref(DerefExpr {
            pointer: Box::new(pointer),
            span,
        });

        Ok(Expression::new(kind, types))
    }
}

impl ExpressionKindImpl for DerefExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        if let Some(target_type) = self.pointer.type_().borrow().pointer_target_type(types) {
            target_type
        } else {
            panic!(
                "DerefExpr is in an invalid state: pointer expression type is `{}`",
                self.pointer.type_().borrow().name
            )
        }
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Lvalue
    }

    fn span(&self) -> Option<InputSpan> {
        self.span
    }
}
