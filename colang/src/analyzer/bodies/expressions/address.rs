use super::compile_expression;
use crate::errors::CompilationError;
use crate::program::{Type, ValueCategory};
use crate::source::SourceOrigin;
use crate::{ast, program, CompilerContext};
use std::cell::RefCell;
use std::rc::Rc;

pub(crate) fn compile_address_expr(
    expression: ast::AddressExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let hint =
        type_hint.and_then(|hint| hint.borrow().pointer_target_type(context.program.types()));

    let target = compile_expression(*expression.target, hint, context);

    if target.value_category() != ValueCategory::Lvalue {
        let error = CompilationError::address_of_rvalue(target.location());
        context.errors.push(error);
        return program::Expression::error(expression.span);
    }

    program::Expression::new(
        program::ExpressionKind::Address(program::AddressExpr {
            target: Box::new(target),
            location: SourceOrigin::Plain(expression.span),
        }),
        context.program.types_mut(),
    )
}
