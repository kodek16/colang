//! Definitions related to tokens.

use crate::source::InputSpan;

/// All information carried by a token except the input span that it covers.
pub trait TokenPayload {
    /// Create an instance that signifies a tokenizer error.
    fn error() -> Self;
}

/// Represents a token: a sequence of characters on the input atomic from a parser perspective.
#[derive(Clone)]
pub struct Token<Payload: TokenPayload> {
    pub span: InputSpan,
    pub payload: Payload,
}

impl<Payload: TokenPayload> Token<Payload> {
    pub fn new(span: InputSpan, payload: Payload) -> Token<Payload> {
        Token { span, payload }
    }
}
