//! Parser for code blocks (`{ ... }`).

use crate::ast;
use crate::parser::prelude::*;
use crate::parser::stmt_or_expr::StmtOrExpr;

pub struct Block;

impl Parser for Block {
    type N = ast::BlockExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq3<AbortIfMissing<CharsParser<LeftBrace>>, Optional<StmtOrExpr>, RightBraceItem>>::parse(
            input, ctx,
        )
        .map(|(left, stmt_or_expr, right)| ast::BlockExpr {
            span: left + right,
            items: stmt_or_expr.into_iter().collect(),
        })
    }
}

struct RightBraceItem;

impl SynthesizeIfMissing for RightBraceItem {
    type P = CharsParser<RightBrace>;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}
