use super::compile_expression;
use crate::context::CompilerContext;
use crate::errors::CompilationError;
use crate::program::{ExpressionKind, Type, ValueCategory};
use crate::source::SourceOrigin;
use crate::{ast, program};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_address_expr(
    expression: ast::AddressExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let hint = type_hint.and_then(|hint| hint.borrow().pointer_target_type());

    let target = compile_expression(*expression.target, hint, context);

    if target.value_category() != ValueCategory::Lvalue {
        let error = CompilationError::address_of_rvalue(target.location());
        context.errors.push(error);
        return program::Expression::error(expression.span);
    }

    program::Expression::new(
        program::AddressExpr {
            target: Box::new(target),
            location: SourceOrigin::Plain(expression.span),
        },
        context.program.types_mut(),
    )
}
