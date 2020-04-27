use crate::analyzer::bodies::{check_argument_types, compile_arguments};
use crate::context::CompilerContext;
use crate::program::Function;
use crate::source::SourceOrigin;
use crate::{ast, program};
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

    if check_argument_types(&function, &arguments, expression.span, context).is_err() {
        return program::Expression::error(expression.span);
    }

    program::Expression::new(
        program::ExpressionKind::Call(program::CallExpr {
            function,
            arguments,
            location: SourceOrigin::Plain(expression.span),
        }),
        context.program.types_mut(),
    )
}
