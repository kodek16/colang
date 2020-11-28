//! Assignment statement parser.

use crate::ast;
use crate::parser::prelude::*;
use crate::parser::stmt_or_expr::{ExprLike, ExprLikeOrSynthesize};

pub struct AssignStmt;

impl Parser for AssignStmt {
    type N = ast::StmtOrExpr;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq3<AbortIfMissing<ExprLike>, AbortIfMissing<SingleEqual>, ExprLikeOrSynthesize>>::parse(
            input,
        )
        .map(|(lhs, _, rhs)| {
            ast::StmtOrExpr::Assign(ast::AssignStmt {
                span: lhs.span() + rhs.span(),
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            })
        })
    }
}
