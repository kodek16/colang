use super::compile_expression;
use crate::analyzer::bodies::call_and_remember;
use crate::errors::CompilationError;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_index_expr(
    expression: ast::IndexExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let collection = compile_expression(*expression.collection, None, context);
    let collection_type = Rc::clone(collection.type_());
    let collection_type = collection_type.borrow();

    let index = compile_expression(*expression.index, None, context);

    if collection.is_error() || index.is_error() {
        return program::Expression::error(expression.span);
    }

    let method = collection_type.lookup_method("index", expression.span);
    let method = match method {
        Ok(method) => Rc::clone(&method),
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    let pointer = call_and_remember(method, vec![collection, index], expression.span, context);
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
                collection_type.name(),
                pointer_type.borrow().name(),
                expression.span,
            );
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
