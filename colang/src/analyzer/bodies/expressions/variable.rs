use crate::{ast, program, CompilerContext};

pub fn compile_variable_expr(
    expression: ast::VariableExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let name = expression.name;

    let variable = context.scope.lookup_variable(&name.text, expression.span);
    match variable {
        Ok(variable) if variable.borrow().type_.borrow().is_error() => {
            program::Expression::error(expression.span)
        }
        Ok(variable) => {
            program::VariableExpr::new(variable, context.program.types_mut(), expression.span)
        }
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
