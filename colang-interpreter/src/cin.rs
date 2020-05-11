//! CO standard input reader implementation.
//!
//! Interaction with standard input in CO is done through `read` and `readln` statements. Simply
//! put, `read` reads by word and `readln` reads by line. Every line provided on the standard input
//! will be handled by either `read` or `readln`, but never both.

use std::io::{self, BufRead};

/// Buffered standard input handler.
pub struct Cin {
    remaining_words: Vec<String>,
}

impl Cin {
    /// Creates a new handler that can be used to start reading standard input.
    pub fn new() -> Cin {
        Cin {
            remaining_words: vec![],
        }
    }

    /// Reads the next "word" from the input.
    ///
    /// Words are delimited by ASCII whitespace characters (`'\t'`, `' '`, `'\n'`).
    ///
    /// If an input line contains multiple words, once the first of them is read, the
    /// rest can be only read through `read_word` as well. A call to `read_line` would
    /// drop them and read the next line.
    pub fn read_word(&mut self) -> Result<String, io::Error> {
        while self.remaining_words.is_empty() {
            self.read_line_for_words()?
        }

        Ok(self.remaining_words.pop().unwrap())
    }

    /// Reads the next "clean" line from the input.
    ///
    /// If any words from the previous line have not been read yet, they are dropped.
    pub fn read_line(&mut self) -> Result<String, io::Error> {
        self.remaining_words.clear();

        let mut line = String::new();
        while line == "" {
            self.read_line_exact(&mut line)?;

            // Remove the newline character that is present in all lines except the last one.
            if line.ends_with('\n') {
                line.pop();
            }
        }

        Ok(line)
    }

    fn read_line_for_words(&mut self) -> Result<(), io::Error> {
        let mut new_line = String::new();
        self.read_line_exact(&mut new_line)?;

        self.remaining_words = new_line
            .split_ascii_whitespace()
            .map(String::from)
            .collect();

        // So we can pop off words from the end.
        self.remaining_words.reverse();
        Ok(())
    }

    fn read_line_exact(&mut self, target: &mut String) -> Result<(), io::Error> {
        let stdin = io::stdin();
        let mut stdin = stdin.lock();
        let read_bytes = stdin.read_line(target)?;

        if read_bytes > 0 {
            Ok(())
        } else {
            Err(io::Error::new(
                io::ErrorKind::UnexpectedEof,
                "not enough input provided",
            ))
        }
    }
}
