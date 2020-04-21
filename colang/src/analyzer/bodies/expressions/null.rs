use crate::errors::CompilationError;
use crate::program::Type;
use crate::source::SourceOrigin;
use crate::{ast, program, CompilerContext};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_null_expr(
    expression: ast::NullExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    let span = expression.span;
    let target_type_hint = type_hint.and_then(|hint| {
        hint.borrow()
            .pointer_target_type(context.program.types_mut())
    });

    match target_type_hint {
        Some(target_type) => {
            let kind = program::ExpressionKind::Null(program::NullExpr {
                target_type,
                location: SourceOrigin::Plain(expression.span),
            });
            program::Expression::new(kind, context.program.types_mut())
        }
        None => {
            let error = CompilationError::cannot_infer_null_pointer_type(SourceOrigin::Plain(span));
            context.errors.push(error);
            program::Expression::error(span)
        }
    }
}
