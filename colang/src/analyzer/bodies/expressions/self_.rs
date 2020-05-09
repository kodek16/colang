use crate::context::CompilerContext;
use crate::errors;
use crate::source::SourceOrigin;
use crate::{ast, program};
use std::rc::Rc;

pub fn compile_self_expr(
    expression: ast::SelfExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    match context.local().self_ {
        Some(ref variable) => program::Expression::new(
            program::VariableExpr {
                variable: Rc::clone(&variable),
                location: SourceOrigin::Plain(expression.span),
            },
            context.program.types_mut(),
        ),
        None => {
            let error = errors::self_in_function_body(SourceOrigin::Plain(expression.span));
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
