use crate::analyzer::bodies::{call_and_remember, compile_arguments};
use crate::program::Function;
use crate::{ast, program, CompilerContext};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_call_expr(
    expression: ast::CallExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let function_name = expression.function_name.text;
    let function_name_span = expression.function_name.span;

    let function = context
        .scope
        .lookup_function(&function_name, function_name_span)
        .map(Rc::clone);

    let function: Rc<RefCell<Function>> = match function {
        Ok(function) => function,
        Err(error) => {
            context.errors.push(error);
            return program::Expression::error(expression.span);
        }
    };

    let arguments = compile_arguments(
        expression.arguments.into_iter(),
        function.borrow().parameters.iter(),
        context,
    );

    let result = call_and_remember(function, arguments, expression.span, context);
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
