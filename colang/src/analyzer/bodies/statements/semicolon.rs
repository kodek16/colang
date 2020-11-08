use crate::ast;
use crate::context::CompilerContext;
use crate::program;
use crate::program::BlockBuilder;
use crate::source::SourceOrigin;

pub fn compile_semicolon_stmt(
    statement: ast::SemicolonStmt,
    current_block: &mut BlockBuilder,
    _: &mut CompilerContext,
) {
    current_block.append_statement(program::SemicolonStmt {
        location: SourceOrigin::Plain(statement.span),
    });
}
