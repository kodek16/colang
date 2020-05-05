use crate::analyzer::bodies::expressions::compile_expression;
use crate::context::CompilerContext;
use crate::program::BlockBuilder;
use crate::{ast, program};

pub fn compile_expr_stmt(
    statement: ast::ExprStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let expression = compile_expression(statement.expression, None, context);
    if expression.is_error() {
        return;
    }

    let instruction = program::Instruction::Eval(program::EvalInstruction { expression });
    current_block.append_instruction(instruction);
}
