use crate::analyzer::bodies::check_condition_is_bool;
use crate::analyzer::bodies::expressions::compile_expression;
use crate::errors::CompilationError;
use crate::program::BlockBuilder;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_while_stmt(
    statement: ast::WhileStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let body_span = statement.body.span();
    let cond = compile_expression(
        *statement.cond,
        Some(Rc::clone(context.program.types().bool())),
        context,
    );
    let body = compile_expression(*statement.body, None, context);

    let body_type = body.type_();
    if *body_type != *context.program.types().void() {
        let error = CompilationError::while_body_not_void(&body_type.borrow().name, body_span);
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
    });
    current_block.append_instruction(instruction);
}
