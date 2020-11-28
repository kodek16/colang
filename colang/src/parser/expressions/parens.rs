//! Parenthesized expression parser.

use crate::ast;
use crate::parser::prelude::*;
use crate::parser::stmt_or_expr::ExprLikeOrSynthesize;

pub struct ParensExpr;

impl Parser for ParensExpr {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq3<AbortIfMissing<LeftParen>, ExprLikeOrSynthesize, RightParenOrSynthesize>>::parse(
            input,
        )
        .map(|(_, expr, _)| expr)
    }
}
