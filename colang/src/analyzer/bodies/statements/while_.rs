use crate::analyzer::bodies::check_condition_is_bool;
use crate::analyzer::bodies::expressions::compile_expression;
use crate::errors::CompilationError;
use crate::program::BlockBuilder;
use crate::source::SourceOrigin;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub(crate) fn compile_while_stmt(
    statement: ast::WhileStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let cond = compile_expression(
        *statement.cond,
        Some(Rc::clone(context.program.types().bool())),
        context,
    );
    let body = compile_expression(*statement.body, None, context);

    if *body.type_() != *context.program.types().void() {
        let error = CompilationError::while_body_not_void(&body);
        context.errors.push(error)
    }
    let body = program::EvalInstruction::new(body);

    if cond.is_error() {
        return;
    }

    check_condition_is_bool(&cond, context);

    let instruction = program::Instruction::While(program::WhileInstruction {
        cond: Box::new(cond),
        body: Box::new(body),
        location: SourceOrigin::Plain(statement.span),
    });
    current_block.append_instruction(instruction);
}
