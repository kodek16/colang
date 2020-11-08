use crate::parser::base::Chars;

pub struct Colon;

pub struct Comma;

pub struct LeftParen;

pub struct RightParen;

pub struct LeftBrace;

pub struct RightBrace;

impl Chars for Colon {
    const CHARS: &'static str = ":";
}

impl Chars for Comma {
    const CHARS: &'static str = ",";
}

impl Chars for LeftParen {
    const CHARS: &'static str = "(";
}

impl Chars for RightParen {
    const CHARS: &'static str = ")";
}

impl Chars for LeftBrace {
    const CHARS: &'static str = "{";
}

impl Chars for RightBrace {
    const CHARS: &'static str = "}";
}
