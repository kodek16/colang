use super::compile_expression;
use crate::analyzer::bodies::check_condition_is_bool;
use crate::context::CompilerContext;
use crate::errors::CompilationError;
use crate::source::SourceOrigin;
use crate::{ast, program};
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

    check_condition_is_bool(&cond, context);

    let then_type = then.type_();

    if else_.is_none() && !then_type.borrow().is_void() {
        let error =
            CompilationError::if_expression_missing_else(&then_type.borrow().name, then.location());
        context.errors.push(error);
        return program::Expression::error(if_.span);
    }

    let else_ = else_.unwrap_or_else(|| {
        program::Expression::empty(SourceOrigin::MissingElse(span), context.program.types())
    });
    let else_type = else_.type_();

    if then_type != else_type {
        let error = CompilationError::if_expression_branch_type_mismatch(
            &then,
            &else_,
            SourceOrigin::Plain(span),
        );
        context.errors.push(error);
        return program::Expression::error(if_.span);
    }

    program::Expression::new(
        program::ExpressionKind::If(program::IfExpr {
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(else_),
            location: SourceOrigin::Plain(span),
        }),
        context.program.types_mut(),
    )
}
