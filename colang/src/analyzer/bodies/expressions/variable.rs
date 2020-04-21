use crate::source::SourceOrigin;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

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
        Ok(variable) => program::Expression::new(
            program::ExpressionKind::Variable(program::VariableExpr {
                variable: Rc::clone(&variable),
                location: SourceOrigin::Plain(expression.span),
            }),
            context.program.types_mut(),
        ),
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
