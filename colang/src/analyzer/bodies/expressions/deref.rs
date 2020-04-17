use super::compile_expression;
use crate::program::Type;
use crate::{ast, program, CompilerContext};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_deref_expr(
    expression: ast::DerefExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let hint = type_hint.map(|hint| context.program.types_mut().pointer_to(&hint));

    let pointer = compile_expression(*expression.pointer, hint, context);

    let result =
        program::DerefExpr::new(pointer, context.program.types_mut(), Some(expression.span));
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
