use crate::analyzer::bodies::expressions::compile_expression_or_statement;
use crate::ast;
use crate::context::CompilerContext;
use crate::program::BlockBuilder;

pub fn compile_expr_stmt(
    statement: ast::ExprStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let expression = compile_expression_or_statement(statement.expression, None, context);
    let statement = expression.into_statement();

    current_block.append_statement(statement);
}
