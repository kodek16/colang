use crate::analyzer::bodies::expressions::compile_expression;
use crate::program::BlockBuilder;
use crate::{ast, program, CompilerContext};

pub fn compile_expr_stmt(
    statement: ast::ExprStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let expression = compile_expression(statement.expression, None, context);
    if expression.is_error() {
        return;
    }

    let result = program::EvalInstruction::new(expression);
    current_block.append_instruction(result);
}
