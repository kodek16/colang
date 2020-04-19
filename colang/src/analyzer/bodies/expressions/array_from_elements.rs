use super::compile_expression;
use crate::errors::CompilationError;
use crate::program::{ExpressionKind, Type};
use crate::{ast, program, CompilerContext};
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
        None => match type_hint.and_then(|hint| hint.borrow().array_element_type(types)) {
            Some(element_type) => element_type,
            None => {
                let error = CompilationError::cannot_infer_empty_type(expression.span);
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
                Some(CompilationError::array_elements_type_mismatch(
                    &inferred_type.borrow().name,
                    &element_type.borrow().name,
                    element
                        .location()
                        .expect("Implicit array element type mismatch"),
                ))
            } else {
                None
            }
        })
        .collect();

    if !type_mismatch_errors.is_empty() {
        context.errors.append(&mut type_mismatch_errors);
        return program::Expression::error(expression.span);
    }

    let kind = ExpressionKind::ArrayFromElements(program::ArrayFromElementsExpr {
        elements,
        element_type: inferred_type,
        location: Some(expression.span),
    });

    program::Expression::new(kind, types)
}
