//! Repetition parser combinators.

use crate::parser::base::WithIgnored;
use crate::parser::common::{ParseResult, ParsedNode, Parser};
use crate::parser::context::ParsingContext;
use crate::parser::input::Input;
use std::marker::PhantomData;

/// A recovery routine that "eats" characters off input until it finds a suitable anchor.
pub trait RecoveryConsumer {
    fn recover<'a>(input: Input<'a>, ctx: &ParsingContext) -> Input<'a>;
}

/// Kleene star (repetition, `*`) parser combinator.
///
/// Consumes input until `P` returns `Missing`, and it is not fixed by a call to `Recover`.
pub struct RepeatZeroOrMore<P: Parser, Recover: RecoveryConsumer> {
    phantom: PhantomData<(P, Recover)>,
}

impl<P: Parser, Recover: RecoveryConsumer> Parser for RepeatZeroOrMore<P, Recover> {
    type N = Vec<P::N>;

    fn parse<'a>(mut input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        let mut result = Vec::new();
        let mut errors = Vec::new();
        let mut recovery_attempted = false;
        loop {
            let ParseResult(node, remaining) = <WithIgnored<P>>::parse(input, ctx);
            input = remaining;

            match node {
                ParsedNode::Ok(node) => {
                    result.push(node);
                    recovery_attempted = false;
                }
                ParsedNode::Recovered(node, mut es) => {
                    errors.append(&mut es);
                    result.push(node);
                    recovery_attempted = false;
                }
                ParsedNode::Missing(error) => {
                    if recovery_attempted {
                        break;
                    } else {
                        errors.push(error);
                        input = Recover::recover(input, ctx);
                        recovery_attempted = true;
                    }
                }
            }
        }
        ParseResult(ParsedNode::new(result, errors), input)
    }
}
