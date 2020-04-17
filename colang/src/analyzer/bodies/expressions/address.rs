use super::compile_expression;
use crate::program::Type;
use crate::{ast, program, CompilerContext};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_address_expr(
    expression: ast::AddressExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let hint =
        type_hint.and_then(|hint| hint.borrow().pointer_target_type(context.program.types()));

    let target = compile_expression(*expression.target, hint, context);

    let result = program::AddressExpr::new(target, context.program.types_mut(), expression.span);
    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(expression.span)
        }
    }
}
