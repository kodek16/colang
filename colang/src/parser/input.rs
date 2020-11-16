//! Keeping track of input that is being parsed.

use crate::parser::tokens::primary::{PrimaryToken, PrimaryTokenizer};
use crate::parser::tokens::queue::TokenQueue;
use crate::source::{InputSpan, InputSpanFile};
use lazy_static::lazy_static;
use std::cell::Cell;
use std::ops::Deref;

/// Represents a view into the program source code at a certain location.
///
/// Tokenizer views are accessed through `Input`, so there is considerable interior mutability
/// going on, but conceptually `Input` is immutable, and all operations on it should be safe.
#[derive(Clone)]
pub struct Input<'a> {
    contents: &'a str,
    offset: usize,
    file: InputSpanFile,

    primary_tokenizer_state: PrimaryTokenizerState,
}

impl<'a> Input<'a> {
    pub fn new(contents: &str, file: InputSpanFile) -> Input {
        Input {
            contents,
            file,
            offset: 0,
            primary_tokenizer_state: PrimaryTokenizerState::new(),
        }
    }

    /// Accesses a view to tokens produced by the primary tokenizer at the current location.
    pub fn with_primary_tokenizer<'b>(&'b self) -> PrimaryTokenizerInputView<'a, 'b> {
        PrimaryTokenizerInputView { input: self }
    }

    /// Creates an `InputSpan` pointing to the first character of the slice.
    pub fn span_of_first(&self) -> InputSpan {
        InputSpan {
            file: self.file,
            start: self.offset,
            end: self.offset + 1,
        }
    }

    pub fn span_of_range(&self, start: usize, end: usize) -> InputSpan {
        InputSpan {
            file: self.file,
            start: self.offset + start,
            end: self.offset + end,
        }
    }
}

impl<'a> Deref for Input<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.contents
    }
}

lazy_static! {
    static ref PRIMARY_TOKENIZER: PrimaryTokenizer = PrimaryTokenizer::new();
}

#[derive(Clone)]
pub struct PrimaryTokenizerState {
    tokens_buffer: TokenQueue<(PrimaryToken, usize)>,
    is_eof: Cell<bool>,
}

impl PrimaryTokenizerState {
    pub fn new() -> PrimaryTokenizerState {
        PrimaryTokenizerState {
            tokens_buffer: TokenQueue::new(),
            is_eof: Cell::new(false),
        }
    }
}

pub struct PrimaryTokenizerInputView<'a, 'b> {
    input: &'b Input<'a>,
}

impl<'a, 'b> PrimaryTokenizerInputView<'a, 'b> {
    /// Returns the token at point of `Input`, or `None` is there are no remaining tokens.
    pub fn peek(&self) -> Option<PrimaryToken> {
        if !self.state().is_eof.get() {
            if let Some((token, _)) = self.state().tokens_buffer.peek() {
                Some(token.clone())
            } else {
                self.tokenize_next();
                self.state().tokens_buffer.peek().map(|(token, _)| token)
            }
        } else {
            None
        }
    }

    /// Returns a new `Input` that points to a location directly after the token returned by `peek`.
    pub fn pop(&self) -> Input<'a> {
        if self.state().tokens_buffer.is_empty() {
            self.tokenize_next();
        }
        assert!(!self.state().is_eof.get());

        let relative_offset = self
            .state()
            .tokens_buffer
            .peek_map(|(_, offset)| offset.clone())
            .unwrap();
        let new_offset = self.input.offset + relative_offset;
        Input {
            file: self.input.file,
            contents: &self.input.contents[new_offset - self.input.offset..],
            offset: new_offset,
            primary_tokenizer_state: PrimaryTokenizerState {
                tokens_buffer: self.state().tokens_buffer.pop(),
                is_eof: Cell::new(false),
            },
        }
    }

    fn tokenize_next(&self) {
        match PRIMARY_TOKENIZER.next(&self.input) {
            Some((token, offset)) => self.state().tokens_buffer.push((token, offset)),
            None => {
                self.state().is_eof.set(true);
            }
        }
    }

    fn state(&self) -> &PrimaryTokenizerState {
        &self.input.primary_tokenizer_state
    }
}
