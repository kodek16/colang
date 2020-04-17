use crate::analyzer::bodies::expressions::compile_expression;
use crate::program::BlockBuilder;
use crate::{ast, program, CompilerContext};

pub fn compile_read_stmt(
    statement: ast::ReadStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    for entry in statement.entries {
        compile_read_entry(entry, current_block, context);
    }
}

fn compile_read_entry(
    entry: ast::ReadEntry,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let target = compile_expression(entry.target, None, context);
    if target.is_error() {
        return;
    }

    let result = program::CallExpr::new_read(target, &mut context.program);
    match result {
        Ok(expression) => {
            current_block.append_instruction(program::EvalInstruction::new(expression))
        }
        Err(error) => context.errors.push(error),
    }
}
