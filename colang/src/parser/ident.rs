//! Parser for identifiers.

use crate::ast;
use crate::parser::prelude::*;
use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref RE: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
}

pub struct Identifier;

impl Parser for Identifier {
    type N = ast::Identifier;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <WithIgnored<Internal>>::parse(input, ctx)
    }
}

struct Internal;

impl Parser for Internal {
    type N = ast::Identifier;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        match input.consume_regex_if_matches(&RE) {
            Some((word, remaining)) => {
                let identifier = ast::Identifier {
                    text: word.to_string(),
                    span: word.span_of_full(ctx),
                };
                ParseResult(ParsedNode::Ok(identifier), remaining)
            }
            None => {
                let error = SyntaxError::UnexpectedToken(input.span_of_first(ctx));
                ParseResult(ParsedNode::Missing(error), input)
            }
        }
    }
}
