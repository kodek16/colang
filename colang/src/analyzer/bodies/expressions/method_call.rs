use super::compile_expression;
use crate::analyzer::bodies::dual::DualNode;
use crate::analyzer::bodies::{check_argument_types, compile_arguments, maybe_deref};
use crate::context::CompilerContext;
use crate::errors;
use crate::program::ValueCategory;
use crate::source::SourceOrigin;
use crate::{ast, program};
use std::rc::Rc;

pub fn compile_method_call(
    expression: ast::MethodCallExpr,
    context: &mut CompilerContext,
) -> DualNode {
    let receiver_span = expression.receiver.span();
    let receiver = compile_expression(*expression.receiver, None, context);
    if receiver.is_error() {
        return DualNode::Expression(program::Expression::error(expression.span));
    }

    // Automatically dereference pointers.
    let receiver = maybe_deref(receiver, context);
    let receiver_type = Rc::clone(receiver.type_());

    let method = receiver_type
        .borrow()
        .lookup_method(&expression.method.text);

    let method = match method {
        Ok(method) => method,
        Err(error) => {
            let error = error.into_direct_lookup_error(SourceOrigin::Plain(expression.method.span));
            context.errors.push(error);
            return DualNode::Expression(program::Expression::error(expression.method.span));
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
            program::Expression::new(
                program::AddressExpr {
                    target: Box::new(receiver),
                    location: SourceOrigin::AddressedForMethodCall(receiver_span),
                },
                context.program.types_mut(),
            )
        } else {
            let error = errors::self_must_be_lvalue(&receiver, &method.borrow());
            context.errors.push(error);
            return DualNode::Expression(program::Expression::error(expression.span));
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

    if check_argument_types(&method, &arguments, expression.span, context).is_err() {
        return DualNode::Expression(program::Expression::error(expression.span));
    }

    DualNode::from_call(
        program::Call {
            function: method,
            arguments,
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
