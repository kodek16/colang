use crate::analyzer::bodies::expressions::compile_expression;
use crate::program::BlockBuilder;
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_assign_stmt(
    statement: ast::AssignStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let lhs = compile_expression(*statement.lhs, None, context);
    let rhs = compile_expression(*statement.rhs, Some(Rc::clone(lhs.type_())), context);
    if lhs.is_error() || rhs.is_error() {
        return;
    }

    let result = program::AssignInstruction::new(lhs, rhs, statement.span);
    match result {
        Ok(statement) => current_block.append_instruction(statement),
        Err(error) => context.errors.push(error),
    }
}
