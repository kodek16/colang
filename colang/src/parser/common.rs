//! Definitions of abstractions used throughout the `parser` module tree.

use crate::parser::context::ParsingContext;
use crate::parser::input::Input;
use crate::source::InputSpan;

/// A syntax error found in the user program.
#[derive(Debug)]
pub enum SyntaxError {
    UnexpectedToken(InputSpan),
}

/// A parser routine that produces a node of type `N`.
///
/// Parsers are typically composed of other parsers, similarly to how rules in grammars
/// derive other, constituent rules.
pub trait Parser {
    type N;
    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N>;
}

/// Node parsed by a parsing routine.
///
/// `Ok` case is the actual node produced by a successful parse. `Recovered` is produced
/// when the parser is sure that some part of the expected node was present and consumed,
/// but the parse was not fully successful. `Missing` appears when the expected node was not
/// found, and nothing was consumed.
pub enum ParsedNode<T> {
    Ok(T),
    Recovered(T, Vec<SyntaxError>),
    Missing(SyntaxError),
}

impl<T> ParsedNode<T> {
    /// Constructs from a node and a list of encountered errors, choosing an appropriate variant.
    pub fn new(t: T, errors: Vec<SyntaxError>) -> ParsedNode<T> {
        if errors.is_empty() {
            ParsedNode::Ok(t)
        } else {
            ParsedNode::Recovered(t, errors)
        }
    }

    pub fn is_ok(&self) -> bool {
        match &self {
            ParsedNode::Ok(_) => true,
            _ => false,
        }
    }
}

/// Result of running a parsing routine.
///
/// First element is the parsed node, second is the remainder of the input that was not consumed
/// by the parse.
pub struct ParseResult<'a, T>(pub ParsedNode<T>, pub Input<'a>);

impl<'a, T> ParseResult<'a, T> {
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> ParseResult<'a, U> {
        let ParseResult(node, input) = self;
        let node = match node {
            ParsedNode::Ok(node) => ParsedNode::Ok(f(node)),
            ParsedNode::Recovered(node, errors) => ParsedNode::Recovered(f(node), errors),
            ParsedNode::Missing(error) => ParsedNode::Missing(error),
        };
        ParseResult(node, input)
    }

    /// If `self` is a successful or recovered parse, adds a syntax error at its end.
    pub fn add_error(self, error: SyntaxError) -> ParseResult<'a, T> {
        let ParseResult(node, input) = self;
        let node = match node {
            ParsedNode::Ok(node) => ParsedNode::Recovered(node, vec![error]),
            ParsedNode::Recovered(node, mut errors) => {
                errors.push(error);
                ParsedNode::Recovered(node, errors)
            }
            m @ ParsedNode::Missing(_) => m,
        };
        ParseResult(node, input)
    }
}
