use crate::ast::InputSpan;
use crate::program::{Expression, ExpressionKind, ValueCategory};
use crate::typing::TypeRegistry;

use crate::errors::CompilationError;
use std::rc::Rc;

#[derive(Debug)]
pub struct ArrayExpr {
    elements: Vec<Expression>,
}

impl ArrayExpr {
    pub fn new(
        elements: Vec<Expression>,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, Vec<CompilationError>> {
        // TODO use type hints instead of assuming `int` for empty array.
        let inferred_type = elements
            .first()
            .map(|element| Rc::clone(&element.type_))
            .unwrap_or_else(|| Rc::clone(types.int()));

        let errors: Vec<_> = elements
            .iter()
            .flat_map(|element| {
                let element_type = &element.type_;
                if *element_type != inferred_type {
                    Some(CompilationError::array_elements_type_mismatch(
                        inferred_type.borrow().name(),
                        element_type.borrow().name(),
                        element.span.expect("Implicit array element type mismatch"),
                    ))
                } else {
                    None
                }
            })
            .collect();

        if !errors.is_empty() {
            return Err(errors);
        }

        let kind = ExpressionKind::Array(ArrayExpr { elements });
        let array_type = types.array_of(&inferred_type);

        Ok(Expression {
            kind,
            type_: array_type,
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        })
    }

    pub fn elements(&self) -> impl Iterator<Item = &Expression> {
        self.elements.iter()
    }
}
