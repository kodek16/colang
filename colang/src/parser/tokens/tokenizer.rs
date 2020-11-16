use crate::parser::input::Input;
use crate::parser::tokens::token::{Token, TokenPayload};
use lazy_static::lazy_static;
use regex::Regex;
use std::marker::PhantomData;

type Handler<T> = Box<dyn Sync + Fn(&str) -> T>;

pub trait TokenizerRules<Payload: TokenPayload> {
    fn rules() -> Vec<(Regex, Handler<Payload>)>;
    fn ignored_rule() -> Regex;
}

pub struct Tokenizer<Payload: TokenPayload, Rules: TokenizerRules<Payload>> {
    rules: Vec<(Regex, Handler<Payload>)>,
    ignored_rule: Regex,
    phantom: PhantomData<Rules>,
}

lazy_static! {
    // Note that this regex is not bound to start of string.
    static ref FALLBACK_RE: Regex = Regex::new(r"(\w+|\S)").unwrap();
}

impl<Payload: TokenPayload, Rules: TokenizerRules<Payload>> Tokenizer<Payload, Rules> {
    pub fn new() -> Self {
        let mut tokenizer = Tokenizer {
            rules: Rules::rules(),
            ignored_rule: Rules::ignored_rule(),
            phantom: PhantomData,
        };
        // `max_by_key` returns the last match if keys are equal, which causes the tokenizer to
        // prefer rules later in the list. To keep the interface as "first longest match", we
        // reverse the rule list internally.
        tokenizer.rules.reverse();
        tokenizer
    }

    pub fn next(&self, input: &Input) -> Option<(Token<Payload>, usize)> {
        let start_offset = match self.ignored_rule.find(&input) {
            Some(match_) => match_.end(),
            None => 0,
        };
        match self
            .rules
            .iter()
            .map(|(re, handler)| {
                re.find(&input[start_offset..])
                    .map(|match_| (match_.range().len(), handler))
            })
            .flatten()
            .max_by_key(|(length, _)| length.clone())
        {
            Some((length, handler)) => {
                let span = input.span_of_range(start_offset, start_offset + length);
                let payload = handler(&input[start_offset..start_offset + length]);
                Some((Token::new(span, payload), start_offset + length))
            }
            None => match FALLBACK_RE.find(&input) {
                Some(match_) => {
                    // Produce an error token.
                    let span = input.span_of_range(match_.start(), match_.end());
                    Some((Token::new(span, Payload::error()), match_.end()))
                }
                None => {
                    // EOF.
                    None
                }
            },
        }
    }
}
