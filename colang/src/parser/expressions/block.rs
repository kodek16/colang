//! Parser for code blocks (`{ ... }`).

use crate::ast;
use crate::parser::prelude::*;
use crate::parser::repeat::RecoverToToken;
use crate::parser::stmt_or_expr::StmtOrExpr;
use crate::parser::tokens::primary::{PrimaryToken, PrimaryTokenPayload};

pub struct BlockExpr;

impl Parser for BlockExpr {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq3<
            AbortIfMissing<LeftBrace>,
            AbortIfMissing<RepeatZeroOrMore<StmtOrExpr, Recover>>,
            RightBraceOrSynthesize,
        >>::parse(input)
        .map(|(left, stmt_or_expr, right)| {
            ast::ExpressionLike::Block(ast::BlockExpr {
                span: left + right,
                items: stmt_or_expr.into_iter().collect(),
            })
        })
    }
}

struct Recover;

impl RecoverToToken for Recover {
    fn is_anchor(token: PrimaryToken) -> bool {
        // TODO: also recover to newline.
        match token.payload {
            PrimaryTokenPayload::Semicolon => true,
            PrimaryTokenPayload::LeftBrace => true,
            PrimaryTokenPayload::RightBrace => true,
            _ => false,
        }
    }
}
