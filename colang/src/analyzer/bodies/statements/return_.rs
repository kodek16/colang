use crate::analyzer::bodies::expressions::compile_expression;
use crate::context::CompilerContext;
use crate::errors::CompilationError;
use crate::program::BlockBuilder;
use crate::source::SourceOrigin;
use crate::{ast, program};
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

    if expression.is_some() && return_type == *context.program.types().void() {
        let error = CompilationError::return_stmt_with_value_in_void_function(
            &function.borrow(),
            expression.as_ref().unwrap(),
        );
        context.errors.push(error);
        return;
    }

    if expression.is_none() && return_type != *context.program.types().void() {
        let error = CompilationError::return_stmt_without_value_in_non_void_function(
            &function.borrow(),
            SourceOrigin::Plain(statement.span),
        );
        context.errors.push(error);
        return;
    }

    let expression = expression.unwrap_or(program::Expression::empty(
        SourceOrigin::MissingReturnValue(statement.span),
        context.program.types(),
    ));

    if expression.is_error() {
        return;
    }

    if *expression.type_() != return_type {
        let error =
            CompilationError::return_statement_type_mismatch(&function.borrow(), &expression);
        context.errors.push(error);
        return;
    }

    let instruction = program::Instruction::Return(program::ReturnInstruction {
        expression,
        location: SourceOrigin::Plain(statement.span),
    });
    current_block.append_instruction(instruction);
}
