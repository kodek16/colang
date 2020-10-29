//! Top-level parser for the entire program.

use crate::ast;
use crate::parser::function_def::FunctionDef;
use crate::parser::prelude::*;
use lazy_static::lazy_static;
use regex::Regex;

pub struct Program;

impl Parser for Program {
    type N = ast::Program;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        let ParseResult(node, input) = <RepeatZeroOrMore<FunctionDef, RecoverToNextGlobal>>::parse(
            input, ctx,
        )
        .map(|functions| ast::Program {
            functions,
            structs: vec![],
            traits: vec![],
        });
        let ParseResult(_, input) = Ignored::parse(input, ctx);
        if input.is_fully_consumed() {
            ParseResult(node, input)
        } else {
            let error = SyntaxError::UnexpectedToken(input.span_of_first(ctx));
            ParseResult(node, input).add_error(error)
        }
    }
}

struct RecoverToNextGlobal;

lazy_static! {
    static ref GLOBAL_ANCHOR_RE: Regex = Regex::new(r"\b(fun|struct|trait)\b").unwrap();
}

impl RecoveryConsumer for RecoverToNextGlobal {
    fn recover<'a>(input: Input<'a>, _: &ParsingContext) -> Input<'a> {
        let (_, remaining) = match GLOBAL_ANCHOR_RE.find(&input) {
            Some(match_) => input.split_at(match_.start()),
            None => input.consume_all(),
        };
        remaining
    }
}
