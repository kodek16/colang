//! Definitions for tokens and tokenizers.
//!
//! A token is an "atomic" sequence of characters in the input that is treated by parsers as
//! indivisible. Tokenizers are routines that split raw input into tokens.
//!
//! Note that there are specific contexts in the grammar (string literals) which require a different
//! tokenizing strategy, and in some cases tokenizers may even be nested. This is why there is
//! not one tokenizer attached to CO input, but multiple, and they scan the input lazily as needed,
//! so that only a viable tokenizer has to scan each part of the source code.

pub mod primary;
pub mod queue;
pub mod string;
pub mod token;
pub mod tokenizer;
