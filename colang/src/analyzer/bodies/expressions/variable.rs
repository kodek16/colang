use crate::context::CompilerContext;
use crate::scope::VariableEntity;
use crate::source::SourceOrigin;
use crate::{ast, program};

pub fn compile_variable_expr(
    expression: ast::VariableExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    let variable = context
        .scope
        .lookup::<VariableEntity>(&expression.name.text);

    match variable {
        Ok(variable) if variable.borrow().type_.borrow().is_error() => {
            program::Expression::error(expression.span)
        }
        Ok(variable) => program::Expression::new(
            program::VariableExpr {
                variable,
                location: SourceOrigin::Plain(expression.span),
            },
            context.program.types_mut(),
        ),
        Err(error) => {
            let error = error.into_direct_lookup_error(SourceOrigin::Plain(expression.span));
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
