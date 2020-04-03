use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, TypeRegistry, ValueCategory};

#[derive(Debug)]
pub struct ArrayFromCopyExpr {
    element: Box<Expression>,
    size: Box<Expression>,
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

        let array_type = types.array_of(&element.type_.borrow());
        let kind = ExpressionKind::ArrayFromCopy(ArrayFromCopyExpr {
            element: Box::new(element),
            size: Box::new(size),
        });

        Ok(Expression {
            kind,
            type_: array_type,
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        })
    }

    pub fn element(&self) -> &Expression {
        &self.element
    }

    pub fn size(&self) -> &Expression {
        &self.size
    }
}
