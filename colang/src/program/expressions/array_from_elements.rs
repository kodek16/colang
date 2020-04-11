use crate::ast::InputSpan;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory};

use crate::errors::CompilationError;
use crate::program::expressions::ExpressionKindImpl;
use std::cell::RefCell;
use std::rc::Rc;

pub struct ArrayFromElementsExpr {
    pub elements: Vec<Expression>,

    // Used for inferring the type of empty arrays.
    pub element_type: Rc<RefCell<Type>>,
}

impl ArrayFromElementsExpr {
    pub fn new(
        elements: Vec<Expression>,
        types: &mut TypeRegistry,
        type_hint: Option<Rc<RefCell<Type>>>,
        span: InputSpan,
    ) -> Result<Expression, Vec<CompilationError>> {
        let inferred_type = elements.first().map(|element| Rc::clone(&element.type_));

        let inferred_type = match inferred_type {
            Some(type_) => type_,
            None => match type_hint.and_then(|hint| hint.borrow().array_element_type(types)) {
                Some(element_type) => element_type,
                None => {
                    let error = CompilationError::cannot_infer_empty_type(span);
                    return Err(vec![error]);
                }
            },
        };

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

        let kind = ExpressionKind::ArrayFromElements(ArrayFromElementsExpr {
            elements,
            element_type: inferred_type,
        });

        Ok(Expression::new(kind, Some(span), types))
    }
}

impl ExpressionKindImpl for ArrayFromElementsExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.array_of(&self.element_type)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }
}
