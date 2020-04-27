use super::compile_expression;
use crate::analyzer::bodies::check_argument_types;
use crate::context::CompilerContext;
use crate::errors::CompilationError;
use crate::source::SourceOrigin;
use crate::{ast, program};
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
    let arguments = vec![collection, index];

    if check_argument_types(&method, &arguments, expression.span, context).is_err() {
        return program::Expression::error(expression.span);
    }

    let pointer = program::Expression::new(
        program::ExpressionKind::Call(program::CallExpr {
            function: method,
            arguments,
            location: SourceOrigin::Plain(expression.span),
        }),
        context.program.types_mut(),
    );

    if pointer.type_().borrow().is_pointer() {
        program::Expression::new(
            program::ExpressionKind::Deref(program::DerefExpr {
                pointer: Box::new(pointer),
                location: SourceOrigin::DereferencedIndex(expression.span),
            }),
            context.program.types_mut(),
        )
    } else {
        let error = CompilationError::index_method_returns_not_pointer(
            &collection_type.borrow().name,
            &pointer.type_().borrow().name,
            SourceOrigin::Plain(expression.span),
        );
        context.errors.push(error);
        program::Expression::error(expression.span)
    }
}
