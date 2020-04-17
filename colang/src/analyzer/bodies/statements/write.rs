use crate::analyzer::bodies::expressions::compile_expression;
use crate::program::BlockBuilder;
use crate::{ast, program, CompilerContext};

pub fn compile_write_stmt(
    statement: ast::WriteStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let expression = compile_expression(statement.expression, None, context);
    if expression.is_error() {
        return;
    }

    let result = program::WriteInstruction::new(expression, &mut context.program);
    match result {
        Ok(instruction) => {
            current_block.append_instruction(instruction);

            if statement.newline {
                let newline =
                    program::LiteralExpr::string("\n", context.program.types_mut(), statement.span)
                        .expect("Couldn't construct '\\n' string literal");
                let instruction = program::WriteInstruction::new(newline, &mut context.program)
                    .expect("Couldn't construct `write` instruction for synthetic newline");
                current_block.append_instruction(instruction)
            }
        }
        Err(error) => context.errors.push(error),
    }
}
