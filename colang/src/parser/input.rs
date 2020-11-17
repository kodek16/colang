//! Keeping track of input that is being parsed.

use crate::parser::tokens::primary::{PrimaryTokenPayload, PrimaryTokenizer};
use crate::parser::tokens::queue::TokenQueue;
use crate::parser::tokens::string::{StringTokenPayload, StringTokenizer};
use crate::parser::tokens::token::{Token, TokenPayload};
use crate::source::{InputSpan, InputSpanFile};
use lazy_static::lazy_static;
use std::cell::Cell;
use std::marker::PhantomData;
use std::ops::Deref;

lazy_static! {
    static ref PRIMARY_TOKENIZER: PrimaryTokenizer = PrimaryTokenizer::new();
    static ref STRING_TOKENIZER: StringTokenizer = StringTokenizer::new();
}

/// Represents a view into the program source code at a certain location.
///
/// Tokenizer views are accessed through `Input`, so there is considerable interior mutability
/// going on, but conceptually `Input` is immutable, and all operations on it should be safe.
#[derive(Clone)]
pub struct Input<'a> {
    contents: &'a str,
    offset: usize,
    file: InputSpanFile,

    primary_tokenizer_state: TokenizerState<PrimaryTokenPayload>,
    string_tokenizer_state: TokenizerState<StringTokenPayload>,
}

impl<'a> Input<'a> {
    pub fn new(contents: &str, file: InputSpanFile) -> Input {
        Input {
            contents,
            file,
            offset: 0,
            primary_tokenizer_state: TokenizerState::new(),
            string_tokenizer_state: TokenizerState::new(),
        }
    }

    /// Accesses a view to tokens produced by the primary tokenizer at the current location.
    pub fn with_primary_tokenizer<'b>(
        &'b self,
    ) -> TokenizerInputView<'a, 'b, PrimaryTokenizerChoice> {
        TokenizerInputView::new(self)
    }

    /// Accesses a view to tokens produced by the string tokenizer at the current location.
    pub fn with_string_tokenizer<'b>(
        &'b self,
    ) -> TokenizerInputView<'a, 'b, StringTokenizerChoice> {
        TokenizerInputView::new(self)
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

    /// Strips characters ignored by the primary tokenizer from the beginning of input.
    ///
    /// This should _not_ be normally used, it is only useful in certain weird contexts, like
    /// switching tokenizers.
    pub fn strip_ignored_prefix_from_primary(self: Input<'a>) -> Input<'a> {
        let offset = PRIMARY_TOKENIZER.ignored_prefix(&self);
        if offset == 0 {
            self
        } else {
            Input {
                contents: &self.contents[offset..],
                offset: self.offset + offset,
                file: self.file,

                // We can't reuse the state because relative offsets would be now invalid.
                // This should be fixable if needed.
                primary_tokenizer_state: TokenizerState::new(),
                string_tokenizer_state: TokenizerState::new(),
            }
        }
    }
}

impl<'a> Deref for Input<'a> {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.contents
    }
}

#[derive(Clone)]
pub struct TokenizerState<Payload: TokenPayload> {
    tokens_buffer: TokenQueue<(Token<Payload>, usize)>,
    is_eof: Cell<bool>,
}

impl<Payload: TokenPayload> TokenizerState<Payload> {
    pub fn new() -> TokenizerState<Payload> {
        TokenizerState {
            tokens_buffer: TokenQueue::new(),
            is_eof: Cell::new(false),
        }
    }
}

/// Abstracts away certain operations with input over a choice of tokenizer.
pub trait TokenizerChoice {
    type Payload: TokenPayload;

    fn state<'a>(input: &'a Input) -> &'a TokenizerState<Self::Payload>;
    fn next(input: &Input) -> Option<(Token<Self::Payload>, usize)>;

    fn advance_primary(view: &Input) -> TokenizerState<PrimaryTokenPayload>;
    fn advance_string(view: &Input) -> TokenizerState<StringTokenPayload>;
}

pub struct PrimaryTokenizerChoice;

impl TokenizerChoice for PrimaryTokenizerChoice {
    type Payload = PrimaryTokenPayload;

    fn state<'a>(input: &'a Input) -> &'a TokenizerState<Self::Payload> {
        &input.primary_tokenizer_state
    }

    fn next(input: &Input) -> Option<(Token<Self::Payload>, usize)> {
        PRIMARY_TOKENIZER.next(input)
    }

    fn advance_primary(input: &Input) -> TokenizerState<PrimaryTokenPayload> {
        TokenizerState {
            tokens_buffer: Self::state(input).tokens_buffer.pop(),
            is_eof: Cell::new(false),
        }
    }

    fn advance_string(_: &Input) -> TokenizerState<StringTokenPayload> {
        TokenizerState::new()
    }
}

pub struct StringTokenizerChoice;

impl TokenizerChoice for StringTokenizerChoice {
    type Payload = StringTokenPayload;

    fn state<'a>(input: &'a Input) -> &'a TokenizerState<Self::Payload> {
        &input.string_tokenizer_state
    }

    fn next(input: &Input) -> Option<(Token<Self::Payload>, usize)> {
        STRING_TOKENIZER.next(input)
    }

    fn advance_primary(_: &Input) -> TokenizerState<PrimaryTokenPayload> {
        TokenizerState::new()
    }

    fn advance_string(input: &Input) -> TokenizerState<StringTokenPayload> {
        TokenizerState {
            tokens_buffer: Self::state(input).tokens_buffer.pop(),
            is_eof: Cell::new(false),
        }
    }
}

pub struct TokenizerInputView<'a, 'b, Choice: TokenizerChoice> {
    input: &'b Input<'a>,
    phantom: PhantomData<Choice>,
}

impl<'a, 'b, Choice: TokenizerChoice> TokenizerInputView<'a, 'b, Choice> {
    pub fn new(input: &'b Input<'a>) -> Self {
        TokenizerInputView {
            input,
            phantom: PhantomData,
        }
    }

    /// Returns the token at point of `Input`, or `None` is there are no remaining tokens.
    pub fn peek(&self) -> Option<Token<Choice::Payload>> {
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
            primary_tokenizer_state: Choice::advance_primary(self.input),
            string_tokenizer_state: Choice::advance_string(self.input),
        }
    }

    fn tokenize_next(&self) {
        match Choice::next(&self.input) {
            Some((token, offset)) => self.state().tokens_buffer.push((token, offset)),
            None => {
                self.state().is_eof.set(true);
            }
        }
    }

    fn state(&self) -> &TokenizerState<Choice::Payload> {
        Choice::state(self.input)
    }
}
