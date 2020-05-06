mod assign;
mod expr;
mod read;
mod return_;
mod var_decl;
mod while_;
mod write;

use crate::ast;
use crate::context::CompilerContext;
use crate::program::expressions::block::BlockBuilder;

pub fn compile_statement(
    statement: ast::Statement,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    match statement {
        ast::Statement::VarDecl(s) => var_decl::compile_var_decl_stmt(s, current_block, context),
        ast::Statement::Read(s) => read::compile_read_stmt(s, current_block, context),
        ast::Statement::Write(s) => write::compile_write_stmt(s, current_block, context),
        ast::Statement::While(s) => while_::compile_while_stmt(s, current_block, context),
        ast::Statement::Assign(s) => assign::compile_assign_stmt(s, current_block, context),
        ast::Statement::Return(s) => return_::compile_return_stmt(s, current_block, context),
        ast::Statement::Expr(s) => expr::compile_expr_stmt(s, current_block, context),
    }
}
