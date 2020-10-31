//! Integer literal parser.

use crate::ast;
use crate::parser::prelude::*;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref RE: Regex = Regex::new(r"^-?[0-9]+\b").unwrap();
}

pub struct IntLiteralExpr;

impl Parser for IntLiteralExpr {
    type N = ast::ExpressionLike;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <WithIgnored<Internal>>::parse(input, ctx)
    }
}

struct Internal;

impl Parser for Internal {
    type N = ast::ExpressionLike;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        match input.consume_regex_if_matches(&RE) {
            Some((word, remaining)) => {
                let span = word.span_of_full(ctx);
                // TODO: this is unsafe, do a checked conversion in the analyzer.
                let value: i32 = word.parse().expect("ERROR: int literal out of range");
                let expr = ast::ExpressionLike::IntLiteral(ast::IntLiteralExpr { value, span });
                ParseResult(ParsedNode::Ok(expr), remaining)
            }
            None => {
                let error = SyntaxError::UnexpectedToken(input.span_of_first(ctx));
                ParseResult(ParsedNode::Missing(error), input)
            }
        }
    }
}
