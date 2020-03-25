//! Buffered standard input reader, providing an interface more similar to C++'s std::cin.

use std::error::Error;
use std::io::{self, BufRead};

pub struct Cin {
    remaining_words: Vec<String>,
}

impl Cin {
    /// Create a new handle that can be used to start reading stdin.
    pub fn new() -> Cin {
        Cin {
            remaining_words: vec![],
        }
    }

    /// Read the next "word" from the input.
    /// If an input line contains multiple words, once the first of them is read, the
    /// rest can be only read through `read_word` as well. A call to `read_line` would
    /// drop them and read the next line.
    pub fn read_word(&mut self) -> Result<String, Box<dyn Error>> {
        while self.remaining_words.is_empty() {
            self.read_line_for_words()?
        }

        Ok(self.remaining_words.pop().unwrap())
    }

    /// Read the next "clean" line from the input.
    /// If any words from the previous line have not been read yet, they are dropped.
    pub fn _read_line(&mut self) -> Result<String, Box<dyn Error>> {
        self.remaining_words.clear();

        let stdin = io::stdin();
        let mut stdin = stdin.lock();

        let mut line = String::new();
        stdin
            .read_line(&mut line)
            .map_err(|_| "Not enough lines given as input")?;
        Ok(line)
    }

    fn read_line_for_words(&mut self) -> Result<(), Box<dyn Error>> {
        let stdin = io::stdin();
        let mut stdin = stdin.lock();

        let mut new_line = String::new();
        stdin
            .read_line(&mut new_line)
            .map_err(|_| "Not enough words given as input")?;
        self.remaining_words = new_line
            .split_ascii_whitespace()
            .map(String::from)
            .collect();

        // So we can pop off words from the end.
        self.remaining_words.reverse();
        Ok(())
    }
}
