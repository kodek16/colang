//! Parsers for `=` and `==` terminals.
//!
//! These have to be handled separately from chars_exact because `=` should not match on the first
//! character of `==`.

use crate::parser::prelude::*;

pub struct SingleEquals;

impl Parser for SingleEquals {
    type N = InputSpan;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <WithIgnored<SingleEqualsInternal>>::parse(input, ctx)
    }
}

struct SingleEqualsInternal;

impl Parser for SingleEqualsInternal {
    type N = InputSpan;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        // TODO report a better error (recover?) if the next token is `==`.
        if input.starts_with("=") && !input.starts_with("==") {
            let (span, remaining) = input.split_at(1);
            ParseResult(ParsedNode::Ok(span.span_of_full(ctx)), remaining)
        } else {
            let error = SyntaxError::UnexpectedToken(input.span_of_first(ctx));
            ParseResult(ParsedNode::Missing(error), input)
        }
    }
}
