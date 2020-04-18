use super::compile_expression;
use crate::errors::CompilationError;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_index_expr(
    expression: ast::IndexExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let collection = compile_expression(*expression.collection, None, context);
    let collection_type = Rc::clone(collection.type_());

    let index = compile_expression(*expression.index, None, context);

    if collection.is_error() || index.is_error() {
        return program::Expression::error(expression.span);
    }

    let method = collection_type
        .borrow()
        .lookup_method("index", expression.span)
        .map(Rc::clone);
    let method = match method {
        Ok(method) => method,
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    let pointer = program::CallExpr::new(
        method,
        vec![collection, index],
        context.program.types_mut(),
        expression.span,
    );
    let pointer = match pointer {
        Ok(pointer) => pointer,
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };
    let pointer_type = Rc::clone(pointer.type_());

    let result =
        program::DerefExpr::new(pointer, context.program.types_mut(), Some(expression.span));
    match result {
        Ok(result) => result,
        Err(_) => {
            let error = CompilationError::index_method_returns_not_pointer(
                &collection_type.borrow().name,
                &pointer_type.borrow().name,
                expression.span,
            );
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
