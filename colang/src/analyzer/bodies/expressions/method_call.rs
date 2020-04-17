use super::compile_expression;
use crate::analyzer::bodies::{call_and_remember, compile_arguments, maybe_deref};
use crate::errors::CompilationError;
use crate::program::ValueCategory;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_method_call_expr(
    expression: ast::MethodCallExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let receiver_span = expression.receiver.span();
    let receiver = compile_expression(*expression.receiver, None, context);
    if receiver.is_error() {
        return program::Expression::error(expression.span);
    }

    // Automatically dereference pointers.
    let receiver = maybe_deref(receiver, context);
    let receiver_type = Rc::clone(receiver.type_());
    let receiver_type = receiver_type.borrow();

    let method = receiver_type.lookup_method(&expression.method.text, expression.method.span);

    let method = match method {
        Ok(method) => Rc::clone(method),
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.method.span);
        }
    };

    let self_parameter = Rc::clone(
        method
            .borrow()
            .parameters
            .get(0)
            .expect("Method does not have a self parameter"),
    );
    let self_type = Rc::clone(&self_parameter.borrow().type_);

    let self_argument = if self_type == *receiver.type_() {
        // self-by-value
        receiver
    } else if self_type == context.program.types_mut().pointer_to(receiver.type_()) {
        // self-by-pointer
        if receiver.value_category() == ValueCategory::Lvalue {
            // TODO handle synthetic span in a special way for errors.
            program::AddressExpr::new_synthetic(
                receiver,
                context.program.types_mut(),
                receiver_span,
            )
        } else {
            let error =
                CompilationError::self_must_be_lvalue(&expression.method.text, receiver_span);
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    } else {
        panic!("Unexpected method `self` type");
    };

    let arguments = {
        let mut other_arguments = compile_arguments(
            expression.arguments.into_iter(),
            method.borrow().parameters.iter().skip(1),
            context,
        );
        let mut arguments = vec![self_argument];
        arguments.append(&mut other_arguments);
        arguments
    };

    let result = call_and_remember(method, arguments, expression.span, context);
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
