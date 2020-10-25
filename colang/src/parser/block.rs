//! Parser for code blocks (`{ ... }`).

use crate::ast;
use crate::parser::prelude::*;

pub struct Block;

impl Parser for Block {
    type N = ast::BlockExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq2<AbortIfMissing<chars::LeftBrace>, RightBraceItem>>::parse(input, ctx).map(
            |(left, right)| ast::BlockExpr {
                span: left + right,
                statements: vec![],
                final_expr: None,
            },
        )
    }
}

struct RightBraceItem;

impl SynthesizeIfMissing for RightBraceItem {
    type P = chars::RightBrace;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}
