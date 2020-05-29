use crate::analyzer::bodies::expressions::compile_expression;
use crate::context::CompilerContext;
use crate::program::expressions::block::BlockBuilder;
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

    current_block.append_statement(program::EvalStmt { expression });
}
