use crate::analyzer::bodies::expressions::compile_expression_or_statement;
use crate::ast;
use crate::context::CompilerContext;
use crate::program::BlockBuilder;

pub fn compile_expr_like(
    expression_like: ast::ExpressionLike,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let node = compile_expression_or_statement(expression_like, None, context);
    let statement = node.into_statement();

    current_block.append_statement(statement);
}
