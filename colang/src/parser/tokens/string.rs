//! String tokenizer definition.
//!
//! String tokenizer is used for `string` and `char` literals.

use crate::parser::tokens::token::{Token, TokenPayload};
use crate::parser::tokens::tokenizer::{Tokenizer, TokenizerRules};
use regex::Regex;

#[derive(Clone)]
pub enum StringTokenPayload {
    SingleQuote,
    DoubleQuote,
    Character(char),
    Error,
}

impl TokenPayload for StringTokenPayload {
    fn error() -> Self {
        StringTokenPayload::Error
    }
}

pub struct StringRules;

impl TokenizerRules<StringTokenPayload> for StringRules {
    fn rules() -> Vec<(Regex, Box<dyn Sync + Fn(&str) -> StringTokenPayload>)> {
        vec![
            (
                Regex::new(r"^'").unwrap(),
                Box::new(|_| StringTokenPayload::SingleQuote),
            ),
            (
                Regex::new(r#"^""#).unwrap(),
                Box::new(|_| StringTokenPayload::DoubleQuote),
            ),
            (
                Regex::new(r"^.").unwrap(),
                Box::new(|c| {
                    let mut chars = c.chars();
                    let first = chars.next().unwrap();
                    assert!(chars.next().is_none());
                    StringTokenPayload::Character(first)
                }),
            ),
        ]
    }

    fn ignored_rule() -> Option<Regex> {
        None
    }
}

pub type StringToken = Token<StringTokenPayload>;
pub type StringTokenizer = Tokenizer<StringTokenPayload, StringRules>;
