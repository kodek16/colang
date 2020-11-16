//! Repetition parser combinators.

use crate::parser::common::{ParseResult, ParsedNode, Parser, SyntaxError};
use crate::parser::input::Input;
use crate::parser::tokens::primary::PrimaryToken;
use std::marker::PhantomData;

/// A recovery routine that drops tokens off input until it finds a suitable anchor.
pub trait RecoveryConsumer {
    fn recover(input: Input) -> Input;
}

/// A no-op recovery routine.
pub struct DontRecover;

impl RecoveryConsumer for DontRecover {
    fn recover(input: Input) -> Input {
        input
    }
}

/// A recovery routine that drops tokens until it finds one that satisfies a predicate.
pub trait RecoverToToken {
    fn is_anchor(token: PrimaryToken) -> bool;
}

impl<R: RecoverToToken> RecoveryConsumer for R {
    fn recover(mut input: Input) -> Input {
        loop {
            let tokens = input.with_primary_tokenizer();
            match tokens.peek() {
                Some(token) => {
                    if R::is_anchor(token) {
                        return input;
                    } else {
                        input = tokens.pop();
                    }
                }
                None => {
                    return input;
                }
            }
        }
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

    fn parse<'a>(mut input: Input<'a>) -> ParseResult<'a, Self::N> {
        let mut result = Vec::new();
        let mut errors = Vec::new();
        let mut recovery_context: Option<RecoveryContext<'a>> = None;
        loop {
            let ParseResult(node, remaining) = P::parse(input);
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
                        recovery_context = Some(RecoveryContext::new(error, input.clone()));
                        input = Recover::recover(input);
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
    pub fn input_before_recovery(self) -> Input<'a> {
        self.saved_input
    }
}
