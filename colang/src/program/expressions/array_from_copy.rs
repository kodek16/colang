use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub struct ArrayFromCopyExpr {
    pub element: Box<Expression>,
    pub size: Box<Expression>,
}

impl ArrayFromCopyExpr {
    pub fn new(
        element: Expression,
        size: Expression,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        let size_type = &size.type_;
        if *size_type != *types.int() {
            let error = CompilationError::array_size_not_int(
                size_type.borrow().name(),
                size.span.expect("Generated array size is not int"),
            );
            return Err(error);
        }

        let kind = ExpressionKind::ArrayFromCopy(ArrayFromCopyExpr {
            element: Box::new(element),
            size: Box::new(size),
        });

        Ok(Expression::new(kind, Some(span), types))
    }
}

impl ExpressionKindImpl for ArrayFromCopyExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.array_of(&self.element.type_())
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }
}
