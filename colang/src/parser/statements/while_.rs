//! `while` statement parser.

use crate::ast;
use crate::parser::expressions::block::BlockExprOrSynthesize;
use crate::parser::prelude::*;
use crate::parser::stmt_or_expr::ExprLikeOrSynthesize;

pub struct WhileStmt;

impl Parser for WhileStmt {
    type N = ast::StmtOrExpr;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq3<AbortIfMissing<KwWhile>, ExprLikeOrSynthesize, BlockExprOrSynthesize>>::parse(input)
            .map(|(kw_while, cond, body)| {
                ast::StmtOrExpr::While(ast::WhileStmt {
                    span: kw_while + body.span(),
                    cond: Box::new(cond),
                    body: Box::new(body),
                })
            })
    }
}
