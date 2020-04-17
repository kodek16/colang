use super::compile_expression;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_if_expr(if_: ast::IfExpr, context: &mut CompilerContext) -> program::Expression {
    let span = if_.span;
    let cond = compile_expression(
        *if_.cond,
        Some(Rc::clone(context.program.types().bool())),
        context,
    );
    let then = compile_expression(*if_.then, None, context);
    let else_ = if_
        .else_
        .map(|else_| compile_expression(*else_, Some(Rc::clone(then.type_())), context));

    let result = program::IfExpr::new(cond, then, else_, context.program.types_mut(), span);

    match result {
        Ok(expression) => expression,
        Err(error) => {
            context.errors.push(error);
            program::Expression::error(if_.span)
        }
    }
}
