use crate::errors::CompilationError;
use crate::{ast, program, CompilerContext};

pub fn compile_self_expr(
    expression: ast::SelfExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    match context.self_ {
        Some(ref variable) => {
            program::VariableExpr::new(variable, context.program.types_mut(), expression.span)
        }
        None => {
            let error = CompilationError::self_in_function_body(expression.span);
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}