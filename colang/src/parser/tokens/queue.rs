//! Persistent and shareable token queues used to store tokenizer output.
//!
//! Tokenizers in CO are lazy: they only parse tokens when explicitly asked by a parser. As the
//! same input characters may be visited by multiple parsers (when iterating through alternatives),
//! the tokenizer output is cached: the tokens are stored in a persistent queue. This allows
//! parsers to reuse already tokenized output after backtracking for whatever reason.
//!
//! Note that the queue uses interior mutability extensively, but semantically it cannot be
//! "changed" by any public method.

use std::cell::RefCell;
use std::rc::Rc;

/// Persistent and shareable token queue.
#[derive(Clone)]
pub struct TokenQueue<T: Clone> {
    contents: Rc<RefCell<Vec<T>>>,
    next: usize,
}

impl<T: Clone> TokenQueue<T> {
    /// Creates an empty token queue.
    pub fn new() -> TokenQueue<T> {
        TokenQueue {
            contents: Rc::new(RefCell::new(Vec::new())),
            next: 0,
        }
    }

    /// Checks if the token queue is empty.
    pub fn is_empty(&self) -> bool {
        self.next == self.contents.borrow().len()
    }

    /// Returns the front element of the queue if it is present.
    pub fn peek(&self) -> Option<T> {
        self.contents.borrow().get(self.next).map(T::clone)
    }

    /// Returns a function of the front element of the queue if it is present.
    ///
    /// Can be used to avoid unnecessarily cloning a token.
    pub fn peek_map<U>(&self, f: impl FnOnce(&T) -> U) -> Option<U> {
        self.contents.borrow().get(self.next).map(f)
    }

    /// Pushes a token to the back of the queue.
    ///
    /// This operation affects all queues that share storage with the receiver.
    pub fn push(&self, value: T) {
        self.contents.borrow_mut().push(value);
    }

    /// Returns a view into the queue that starts with the second element.
    pub fn pop(&self) -> TokenQueue<T> {
        assert!(!self.is_empty());
        TokenQueue {
            contents: Rc::clone(&self.contents),
            next: self.next + 1,
        }
    }
}
