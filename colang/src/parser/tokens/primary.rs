//! Primary tokenizer definitions.
//!
//! Primary tokenizer is used most of the time, except in some special cases like string literals.

use crate::parser::tokens::token::{Token, TokenPayload};
use crate::parser::tokens::tokenizer::{Tokenizer, TokenizerRules};
use regex::Regex;

#[derive(Clone)]
pub enum PrimaryTokenPayload {
    KwElse,
    KwFalse,
    KwFun,
    KwIf,
    KwTrue,
    KwVar,
    Ident(String),
    Int(i32),
    Colon,
    Semicolon,
    Comma,
    Plus,
    Minus,
    Asterisk,
    Slash,
    DoubleEquals,
    SingleEquals,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Error,
}

impl TokenPayload for PrimaryTokenPayload {
    fn error() -> Self {
        PrimaryTokenPayload::Error
    }
}

pub struct PrimaryRules;

impl TokenizerRules<PrimaryTokenPayload> for PrimaryRules {
    fn rules() -> Vec<(Regex, Box<dyn Sync + Fn(&str) -> PrimaryTokenPayload>)> {
        vec![
            (
                Regex::new(r"^else\b").unwrap(),
                Box::new(|_| PrimaryTokenPayload::KwElse),
            ),
            (
                Regex::new(r"^false\b").unwrap(),
                Box::new(|_| PrimaryTokenPayload::KwFalse),
            ),
            (
                Regex::new(r"^fun\b").unwrap(),
                Box::new(|_| PrimaryTokenPayload::KwFun),
            ),
            (
                Regex::new(r"^if\b").unwrap(),
                Box::new(|_| PrimaryTokenPayload::KwIf),
            ),
            (
                Regex::new(r"^true\b").unwrap(),
                Box::new(|_| PrimaryTokenPayload::KwTrue),
            ),
            (
                Regex::new(r"^var\b").unwrap(),
                Box::new(|_| PrimaryTokenPayload::KwVar),
            ),
            (
                Regex::new(r"^[A-Za-z_][A-Za-z0-9_]*\b").unwrap(),
                Box::new(|text| PrimaryTokenPayload::Ident(String::from(text))),
            ),
            (
                Regex::new(r"^-?[0-9]+\b").unwrap(),
                // TODO: handle overflows.
                Box::new(|text| PrimaryTokenPayload::Int(text.parse().unwrap())),
            ),
            (
                Regex::new(r"^:").unwrap(),
                Box::new(|_| PrimaryTokenPayload::Colon),
            ),
            (
                Regex::new(r"^;").unwrap(),
                Box::new(|_| PrimaryTokenPayload::Semicolon),
            ),
            (
                Regex::new(r"^,").unwrap(),
                Box::new(|_| PrimaryTokenPayload::Comma),
            ),
            (
                Regex::new(r"^\+").unwrap(),
                Box::new(|_| PrimaryTokenPayload::Plus),
            ),
            (
                Regex::new(r"^-").unwrap(),
                Box::new(|_| PrimaryTokenPayload::Minus),
            ),
            (
                Regex::new(r"^\*").unwrap(),
                Box::new(|_| PrimaryTokenPayload::Asterisk),
            ),
            (
                Regex::new(r"^/").unwrap(),
                Box::new(|_| PrimaryTokenPayload::Slash),
            ),
            (
                Regex::new(r"^==").unwrap(),
                Box::new(|_| PrimaryTokenPayload::DoubleEquals),
            ),
            (
                Regex::new(r"^=").unwrap(),
                Box::new(|_| PrimaryTokenPayload::SingleEquals),
            ),
            (
                Regex::new(r"^<=").unwrap(),
                Box::new(|_| PrimaryTokenPayload::LessEqual),
            ),
            (
                Regex::new(r"^>=").unwrap(),
                Box::new(|_| PrimaryTokenPayload::GreaterEqual),
            ),
            (
                Regex::new(r"^<").unwrap(),
                Box::new(|_| PrimaryTokenPayload::Less),
            ),
            (
                Regex::new(r"^>").unwrap(),
                Box::new(|_| PrimaryTokenPayload::Greater),
            ),
            (
                Regex::new(r"^\(").unwrap(),
                Box::new(|_| PrimaryTokenPayload::LeftParen),
            ),
            (
                Regex::new(r"^\)").unwrap(),
                Box::new(|_| PrimaryTokenPayload::RightParen),
            ),
            (
                Regex::new(r"^\{").unwrap(),
                Box::new(|_| PrimaryTokenPayload::LeftBrace),
            ),
            (
                Regex::new(r"^}").unwrap(),
                Box::new(|_| PrimaryTokenPayload::RightBrace),
            ),
        ]
    }

    fn ignored_rule() -> Regex {
        Regex::new(r"^(\s*|//.*)*").unwrap()
    }
}

pub type PrimaryToken = Token<PrimaryTokenPayload>;
pub type PrimaryTokenizer = Tokenizer<PrimaryTokenPayload, PrimaryRules>;
