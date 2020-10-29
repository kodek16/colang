//! Repetition parser combinators.

use crate::parser::base::WithIgnored;
use crate::parser::common::{ParseResult, ParsedNode, Parser, SyntaxError};
use crate::parser::context::ParsingContext;
use crate::parser::input::Input;
use std::marker::PhantomData;

/// A recovery routine that "eats" characters off input until it finds a suitable anchor.
pub trait RecoveryConsumer {
    fn recover<'a>(input: Input<'a>, ctx: &ParsingContext) -> Input<'a>;
}

/// A no-op recovery routine.
pub struct DontRecover;

impl RecoveryConsumer for DontRecover {
    fn recover<'a>(input: Input<'a>, _: &ParsingContext) -> Input<'a> {
        input
    }
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
        let mut recovery_context: Option<RecoveryContext<'a>> = None;
        loop {
            let ParseResult(node, remaining) = <WithIgnored<P>>::parse(input, ctx);
            input = remaining;

            match node {
                ParsedNode::Ok(node) => {
                    if let Some(context) = recovery_context.take() {
                        errors.push(context.error_before_recovery());
                    }
                    result.push(node);
                }
                ParsedNode::Recovered(node, mut es) => {
                    if let Some(context) = recovery_context.take() {
                        errors.push(context.error_before_recovery());
                    }
                    errors.append(&mut es);
                    result.push(node);
                }
                ParsedNode::Missing(error) => {
                    if let Some(context) = recovery_context.take() {
                        input = context.input_before_recovery();
                        break;
                    } else {
                        recovery_context = Some(RecoveryContext::new(error, input));
                        input = Recover::recover(input, ctx);
                    }
                }
            }
        }
        ParseResult(ParsedNode::new(result, errors), input)
    }
}

struct RecoveryContext<'a> {
    saved_error: SyntaxError,
    saved_input: Input<'a>,
}

impl<'a> RecoveryContext<'a> {
    pub fn new(saved_error: SyntaxError, saved_input: Input) -> RecoveryContext {
        RecoveryContext {
            saved_error,
            saved_input,
        }
    }

    /// Recovery succeeded, save the previous error and continue.
    pub fn error_before_recovery(self) -> SyntaxError {
        self.saved_error
    }

    /// Recovery failed, backtrack to input state before attempting recovery.
    pub fn input_before_recovery<'b>(&'b self) -> Input<'a> {
        self.saved_input
    }
}
