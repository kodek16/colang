//! Keeping track of input that is being parsed.

use crate::parser::context::ParsingContext;
use crate::source::InputSpan;
use regex::Regex;
use std::ops::Deref;

/// Wrapper for &str that preserves absolute indices.
#[derive(Copy, Clone)]
pub struct Input<'a> {
    contents: &'a str,
    offset: usize,
}

impl<'a> Input<'a> {
    pub fn new(contents: &str) -> Input {
        Input {
            contents,
            offset: 0,
        }
    }

    pub fn is_fully_consumed(&self) -> bool {
        self.is_empty()
    }

    /// Splits the underlying slice at an index.
    pub fn split_at(self, mid: usize) -> (Input<'a>, Input<'a>) {
        let (head, tail) = self.contents.split_at(mid);
        let head = Input {
            contents: head,
            offset: self.offset,
        };
        let tail = Input {
            contents: tail,
            offset: self.offset + mid,
        };
        (head, tail)
    }

    /// Splits the underlying slice into a (full, empty) pair.
    pub fn consume_all(self) -> (Input<'a>, Input<'a>) {
        self.split_at(self.len())
    }

    /// Splits off a regex match from the beginning of input, if it matches.
    ///
    /// `re` _must_ start with a `^` anchor.
    pub fn consume_regex_if_matches(self, re: &Regex) -> Option<(Input<'a>, Input<'a>)> {
        re.find(&self).map(|match_| {
            assert_eq!(match_.start(), 0);
            let end = match_.end();
            let (head, tail) = self.contents.split_at(end);
            let head = Input {
                contents: head,
                offset: self.offset,
            };
            let tail = Input {
                contents: tail,
                offset: self.offset + end,
            };
            (head, tail)
        })
    }

    /// Creates an `InputSpan` corresponding to the slice.
    pub fn span_of_full(&self, ctx: &ParsingContext) -> InputSpan {
        InputSpan {
            file: ctx.file,
            start: self.offset,
            end: self.offset + self.len(),
        }
    }

    /// Creates an `InputSpan` pointing to the first character of the slice.
    pub fn span_of_first(&self, ctx: &ParsingContext) -> InputSpan {
        InputSpan {
            file: ctx.file,
            start: self.offset,
            end: self.offset + 1,
        }
    }
}

impl<'a> Deref for Input<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.contents
    }
}
