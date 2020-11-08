mod assign;
mod expr_like;
mod read;
mod return_;
mod semicolon;
mod var_decl;
mod while_;
mod write;

use crate::ast;
use crate::context::CompilerContext;
use crate::program::BlockBuilder;

pub fn compile_statement(
    statement: ast::StmtOrExpr,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    match statement {
        ast::StmtOrExpr::VarDecl(s) => var_decl::compile_var_decl_stmt(s, current_block, context),
        ast::StmtOrExpr::Read(s) => read::compile_read_stmt(s, current_block, context),
        ast::StmtOrExpr::Write(s) => write::compile_write_stmt(s, current_block, context),
        ast::StmtOrExpr::While(s) => while_::compile_while_stmt(s, current_block, context),
        ast::StmtOrExpr::Assign(s) => assign::compile_assign_stmt(s, current_block, context),
        ast::StmtOrExpr::Return(s) => return_::compile_return_stmt(s, current_block, context),
        ast::StmtOrExpr::Semicolon(s) => {
            semicolon::compile_semicolon_stmt(s, current_block, context)
        }
        ast::StmtOrExpr::ExprLike(e) => expr_like::compile_expr_like(e, current_block, context),
    }
}
