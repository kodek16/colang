//! Semicolon parser.

use crate::ast;
use crate::parser::prelude::*;

pub struct SemicolonStmt;

impl Parser for SemicolonStmt {
    type N = ast::StmtOrExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <CharsParser<Semicolon>>::parse(input, ctx)
            .map(|span| ast::StmtOrExpr::Semicolon(ast::SemicolonStmt { span }))
    }
}
