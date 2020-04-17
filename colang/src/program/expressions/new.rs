use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub struct NewExpr {
    pub target_type: Rc<RefCell<Type>>,
    pub span: Option<InputSpan>,
}

impl NewExpr {
    pub fn new(
        target_type: Rc<RefCell<Type>>,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        if target_type == *types.void() {
            let error = CompilationError::new_expression_void_type(span);
            return Err(error);
        }

        let kind = ExpressionKind::New(NewExpr {
            target_type,
            span: Some(span),
        });
        Ok(Expression::new(kind, types))
    }
}

impl ExpressionKindImpl for NewExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.pointer_to(&self.target_type)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn span(&self) -> Option<InputSpan> {
        self.span
    }
}
