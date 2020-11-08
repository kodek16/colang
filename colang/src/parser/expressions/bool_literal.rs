//! Boolean literal parser (parses `true` or `false`).

use crate::ast;
use crate::parser::prelude::*;

pub struct BoolLiteralExpr;

impl Parser for BoolLiteralExpr {
    type N = ast::ExpressionLike;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <OneOf2<TrueParser, FalseParser>>::parse(input, ctx).map(ast::ExpressionLike::BoolLiteral)
    }
}

struct FalseParser;
struct TrueParser;

impl Parser for FalseParser {
    type N = ast::BoolLiteralExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        KwFalse::parse(input, ctx).map(|span| ast::BoolLiteralExpr { value: false, span })
    }
}

impl Parser for TrueParser {
    type N = ast::BoolLiteralExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        KwTrue::parse(input, ctx).map(|span| ast::BoolLiteralExpr { value: true, span })
    }
}
