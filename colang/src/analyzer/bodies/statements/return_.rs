use crate::analyzer::bodies::expressions::compile_expression;
use crate::context::CompilerContext;
use crate::program::expressions::block::BlockBuilder;
use crate::source::SourceOrigin;
use crate::{ast, errors, program};
use std::rc::Rc;

pub fn compile_return_stmt(
    statement: ast::ReturnStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let function = Rc::clone(&context.local().function);
    let return_type = Rc::clone(&function.borrow().return_type);

    let expression = statement
        .expression
        .map(|expr| compile_expression(expr, Some(Rc::clone(&return_type)), context));

    if expression.is_some() && return_type.borrow().is_void() {
        let error =
            errors::return_value_in_void_function(&function.borrow(), expression.as_ref().unwrap());
        context.errors.push(error);
        return;
    }

    if expression.is_none() && !return_type.borrow().is_void() {
        let error = errors::return_no_value_in_non_void_function(
            &function.borrow(),
            SourceOrigin::Plain(statement.span),
        );
        context.errors.push(error);
        return;
    }

    let expression = expression.unwrap_or(program::Expression::new(
        program::EmptyExpr {
            location: SourceOrigin::MissingReturnValue(statement.span),
        },
        context.program.types_mut(),
    ));

    if expression.is_error() {
        return;
    }

    if *expression.type_() != return_type {
        let error = errors::return_statement_type_mismatch(&function.borrow(), &expression);
        context.errors.push(error);
        return;
    }

    current_block.append_instruction(program::ReturnInstruction {
        expression,
        location: SourceOrigin::Plain(statement.span),
    });
}
