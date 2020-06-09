use crate::analyzer::bodies::dual::DualNode;
use crate::analyzer::bodies::{check_argument_types, compile_arguments};
use crate::context::CompilerContext;
use crate::scope::FunctionEntity;
use crate::source::SourceOrigin;
use crate::{ast, program};

pub fn compile_call(expression: ast::CallExpr, context: &mut CompilerContext) -> DualNode {
    let function = context
        .scope
        .lookup::<FunctionEntity>(&expression.function_name.text);

    let function = match function {
        Ok(function) => function,
        Err(error) => {
            let error =
                error.into_direct_lookup_error(SourceOrigin::Plain(expression.function_name.span));
            context.errors.push(error);
            return DualNode::Expression(program::Expression::error(expression.span));
        }
    };

    let arguments = compile_arguments(
        expression.arguments.into_iter(),
        function.borrow().parameters.iter(),
        context,
    );

    if check_argument_types(&function, &arguments, expression.span, context).is_err() {
        return DualNode::Expression(program::Expression::error(expression.span));
    }

    DualNode::from_call(
        program::Call {
            function,
            arguments,
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
