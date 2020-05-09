use crate::analyzer::bodies::check_condition_is_bool;
use crate::analyzer::bodies::expressions::compile_expression;
use crate::context::CompilerContext;
use crate::program::expressions::block::BlockBuilder;
use crate::source::SourceOrigin;
use crate::{ast, errors, program};
use std::rc::Rc;

pub fn compile_while_stmt(
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

    if !body.type_().borrow().is_void() {
        let error = errors::while_body_not_void(&body);
        context.errors.push(error)
    }

    let body = program::Instruction::Eval(program::EvalInstruction { expression: body });

    if cond.is_error() {
        return;
    }

    check_condition_is_bool(&cond, context);

    current_block.append_instruction(program::WhileInstruction {
        cond,
        body: Box::new(body),
        location: SourceOrigin::Plain(statement.span),
    });
}
