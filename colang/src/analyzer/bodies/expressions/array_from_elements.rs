use super::compile_expression;
use crate::context::CompilerContext;
use crate::errors;
use crate::program::Type;
use crate::source::SourceOrigin;
use crate::{ast, program};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_array_from_elements_expr(
    expression: ast::ArrayFromElementsExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let elements: Vec<_> = expression
        .elements
        .into_iter()
        .map(|element| compile_expression(element, None, context))
        .collect();

    let types = context.program.types_mut();

    let inferred_type = elements.first().map(|element| Rc::clone(element.type_()));
    let inferred_type = match inferred_type {
        Some(type_) => type_,
        None => match type_hint.and_then(|hint| hint.borrow().array_element_type()) {
            Some(element_type) => element_type,
            None => {
                let error =
                    errors::cannot_infer_empty_array_type(SourceOrigin::Plain(expression.span));
                context.errors.push(error);
                return program::Expression::error(expression.span);
            }
        },
    };

    let mut type_mismatch_errors: Vec<_> = elements
        .iter()
        .flat_map(|element| {
            let element_type = element.type_();
            if *element_type != inferred_type {
                Some(errors::array_elements_type_mismatch(&elements[0], &element))
            } else {
                None
            }
        })
        .collect();

    if !type_mismatch_errors.is_empty() {
        context.errors.append(&mut type_mismatch_errors);
        return program::Expression::error(expression.span);
    }

    program::Expression::new(
        program::ArrayFromElementsExpr {
            elements,
            element_type: inferred_type,
            location: SourceOrigin::Plain(expression.span),
        },
        types,
    )
}
