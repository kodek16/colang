use crate::parser::base::{Chars, CharsParser};
use crate::parser::seq::SynthesizeIfMissing;
use crate::source::InputSpan;

mod internal {
    pub struct Colon;
    pub struct Semicolon;
    pub struct Comma;
    pub struct LeftParen;
    pub struct RightParen;
    pub struct LeftBrace;
    pub struct RightBrace;
}

impl Chars for internal::Colon {
    const CHARS: &'static str = ":";
}

impl Chars for internal::Semicolon {
    const CHARS: &'static str = ";";
}

impl Chars for internal::Comma {
    const CHARS: &'static str = ",";
}

impl Chars for internal::LeftParen {
    const CHARS: &'static str = "(";
}

impl Chars for internal::RightParen {
    const CHARS: &'static str = ")";
}

impl Chars for internal::LeftBrace {
    const CHARS: &'static str = "{";
}

impl Chars for internal::RightBrace {
    const CHARS: &'static str = "}";
}

pub type Colon = CharsParser<internal::Colon>;
pub type Semicolon = CharsParser<internal::Semicolon>;
pub type Comma = CharsParser<internal::Comma>;
pub type LeftParen = CharsParser<internal::LeftParen>;
pub type RightParen = CharsParser<internal::RightParen>;
pub type LeftBrace = CharsParser<internal::LeftBrace>;
pub type RightBrace = CharsParser<internal::RightBrace>;

pub struct ColonOrSynthesize;
pub struct LeftParenOrSynthesize;
pub struct RightParenOrSynthesize;
pub struct RightBraceOrSynthesize;

impl SynthesizeIfMissing for ColonOrSynthesize {
    type P = Colon;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}

impl SynthesizeIfMissing for LeftParenOrSynthesize {
    type P = LeftParen;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}

impl SynthesizeIfMissing for RightParenOrSynthesize {
    type P = RightParen;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}

impl SynthesizeIfMissing for RightBraceOrSynthesize {
    type P = RightBrace;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}
