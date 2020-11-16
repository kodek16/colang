//! Boolean literal parser (parses `true` or `false`).

use crate::ast;
use crate::parser::prelude::*;

pub struct BoolLiteralExpr;

impl Parser for BoolLiteralExpr {
    type N = ast::ExpressionLike;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <OneOf2<TrueParser, FalseParser>>::parse(input).map(ast::ExpressionLike::BoolLiteral)
    }
}

struct FalseParser;
struct TrueParser;

impl Parser for FalseParser {
    type N = ast::BoolLiteralExpr;

    fn parse(input: Input) -> ParseResult<Self::N> {
        KwFalse::parse(input).map(|span| ast::BoolLiteralExpr { value: false, span })
    }
}

impl Parser for TrueParser {
    type N = ast::BoolLiteralExpr;

    fn parse(input: Input) -> ParseResult<Self::N> {
        KwTrue::parse(input).map(|span| ast::BoolLiteralExpr { value: true, span })
    }
}
