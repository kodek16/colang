//! Definitions of abstractions used throughout the `parser` module tree.

use crate::parser::input::{Input, TokenizerChoice, TokenizerInputView};
use crate::parser::tokens::primary::PrimaryToken;
use crate::parser::tokens::string::StringToken;
use crate::parser::tokens::token::Token;
use crate::source::InputSpan;

/// A syntax error found in the user program.
#[derive(Debug)]
pub enum SyntaxError {
    UnexpectedToken(InputSpan),
    UnexpectedEOF(InputSpan),
    StatementInExprContext(InputSpan),
}

/// A parser routine that produces a node of type `N`.
///
/// Parsers are typically composed of other parsers, similarly to how rules in grammars
/// derive other, constituent rules.
pub trait Parser {
    type N;
    fn parse(input: Input) -> ParseResult<Self::N>;
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

    /// If `self` is a successful or recovered parse, adds a syntax error at its end.
    pub fn add_error(self, error: SyntaxError) -> ParsedNode<T> {
        match self {
            ParsedNode::Ok(node) => ParsedNode::Recovered(node, vec![error]),
            ParsedNode::Recovered(node, mut errors) => {
                errors.push(error);
                ParsedNode::Recovered(node, errors)
            }
            m @ ParsedNode::Missing(_) => m,
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

    pub fn bind<U>(self, f: impl FnOnce(T) -> ParsedNode<U>) -> ParseResult<'a, U> {
        let ParseResult(node, input) = self;
        let node = match node {
            ParsedNode::Ok(node) => f(node),
            ParsedNode::Recovered(node, errors) => errors
                .into_iter()
                .fold(f(node), |acc, err| acc.add_error(err)),
            ParsedNode::Missing(error) => ParsedNode::Missing(error),
        };
        ParseResult(node, input)
    }

    /// If `self` is a successful or recovered parse, adds a syntax error at its end.
    pub fn add_error(self, error: SyntaxError) -> ParseResult<'a, T> {
        let ParseResult(node, input) = self;
        ParseResult(node.add_error(error), input)
    }
}

/// Can be used at tokenizer boundaries to strip characters ignored by the primary tokenizer.
pub struct SkipIgnoredByPrimaryTokenizer;

impl Parser for SkipIgnoredByPrimaryTokenizer {
    type N = ();

    fn parse(input: Input) -> ParseResult<Self::N> {
        let input = input.strip_ignored_prefix_from_primary();
        ParseResult(ParsedNode::Ok(()), input)
    }
}

/// Helper function for typical single-token parsers using the primary tokenizer.
pub fn parse_from_next_primary_token<N>(
    input: Input,
    f: impl FnOnce(PrimaryToken) -> Option<N>,
) -> ParseResult<N> {
    parse_from_next_token(input, Input::with_primary_tokenizer, f)
}

/// Helper function for typical single-token parsers using the string tokenizer.
pub fn parse_from_next_string_token<N>(
    input: Input,
    f: impl FnOnce(StringToken) -> Option<N>,
) -> ParseResult<N> {
    parse_from_next_token(input, Input::with_string_tokenizer, f)
}

fn parse_from_next_token<'a, N, Choice: TokenizerChoice>(
    input: Input<'a>,
    to_tokens: impl for<'b> FnOnce(&'b Input<'a>) -> TokenizerInputView<'a, 'b, Choice>,
    f: impl FnOnce(Token<Choice::Payload>) -> Option<N>,
) -> ParseResult<N> {
    let tokens = to_tokens(&input);
    match tokens.peek() {
        Some(token) => {
            let span = token.span;
            match f(token) {
                Some(node) => ParseResult(ParsedNode::Ok(node), tokens.pop()),
                None => {
                    let error = SyntaxError::UnexpectedToken(span);
                    ParseResult(ParsedNode::Missing(error), input)
                }
            }
        }
        None => {
            let error = SyntaxError::UnexpectedEOF(input.span_of_first());
            ParseResult(ParsedNode::Missing(error), input)
        }
    }
}
