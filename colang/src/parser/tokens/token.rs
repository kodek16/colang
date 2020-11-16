use crate::source::InputSpan;

pub trait TokenPayload {
    fn error() -> Self;
}

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
