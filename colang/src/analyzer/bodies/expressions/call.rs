use crate::analyzer::bodies::{check_argument_types, compile_arguments};
use crate::context::CompilerContext;
use crate::scope::FunctionEntity;
use crate::source::SourceOrigin;
use crate::{ast, program};

pub fn compile_call_expr(
    expression: ast::CallExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let function_name = expression.function_name.text;
    let function_name_span = expression.function_name.span;

    let function = context
        .scope
        .lookup::<FunctionEntity>(&function_name, SourceOrigin::Plain(function_name_span));

    let function = match function {
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
        program::CallExpr {
            function,
            arguments,
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
