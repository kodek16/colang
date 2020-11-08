//! Parser for code blocks (`{ ... }`).

use crate::ast;
use crate::parser::prelude::*;
use crate::parser::stmt_or_expr::StmtOrExpr;

pub struct Block;

impl Parser for Block {
    type N = ast::BlockExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq3<
            AbortIfMissing<LeftBrace>,
            AbortIfMissing<RepeatZeroOrMore<StmtOrExpr, Recover>>,
            RightBraceOrSynthesize,
        >>::parse(input, ctx)
        .map(|(left, stmt_or_expr, right)| ast::BlockExpr {
            span: left + right,
            items: stmt_or_expr.into_iter().collect(),
        })
    }
}

struct Recover;

impl RecoveryConsumer for Recover {
    fn recover<'a>(input: Input<'a>, _: &ParsingContext) -> Input<'a> {
        static ANCHORS: [char; 4] = [';', '\n', '{', '}'];
        match input.find(&ANCHORS[..]) {
            Some(idx) => input.split_at(idx).1,
            None => input,
        }
    }
}
