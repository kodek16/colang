use super::compile_expression;
use crate::errors::CompilationError;
use crate::source::SourceOrigin;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub(crate) fn compile_is_expr(
    expression: ast::IsExpr,
    context: &mut CompilerContext,
) -> program::Expression {
    // TODO find a way to infer type in the opposite direction (rhs -> lhs) as well.
    let lhs = compile_expression(*expression.lhs, None, context);
    let rhs = compile_expression(*expression.rhs, Some(Rc::clone(lhs.type_())), context);

    if lhs.is_error() || rhs.is_error() {
        return program::Expression::error(expression.span);
    }

    let mut had_errors = false;
    for operand in &[&lhs, &rhs] {
        if !operand.type_().borrow().is_pointer() {
            let error = CompilationError::is_expr_operand_wrong_type(operand);
            context.errors.push(error);
            had_errors = true;
        }
    }

    if !had_errors {
        if lhs.type_() != rhs.type_() {
            let error = CompilationError::is_expr_type_mismatch(
                &lhs,
                &rhs,
                SourceOrigin::Plain(expression.span),
            );
            context.errors.push(error);
        }
    }

    let kind = program::ExpressionKind::Is(program::IsExpr {
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        location: SourceOrigin::Plain(expression.span),
    });
    program::Expression::new(kind, context.program.types_mut())
}
