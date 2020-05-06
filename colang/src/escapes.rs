//! Escape sequence handling for character and string literals.

use crate::errors::CompilationError;
use crate::source::{InputSpan, SourceOrigin};

pub fn unescape(text: &str, span: InputSpan) -> Result<Vec<u8>, CompilationError> {
    let mut result: Vec<u8> = Vec::new();

    let mut text_iter = text.as_bytes().iter();
    while let Some(character) = text_iter.next() {
        if *character == b'\\' {
            let next_character = text_iter
                .next()
                .expect("Unterminated escape sequence in literal");
            let escaped_character = match *next_character {
                b'\'' => b'\'',
                b'"' => b'"',
                b'n' => b'\n',
                b'r' => b'\r',
                b't' => b'\t',
                b'0' => b'\0',
                _ => {
                    let error = CompilationError::unknown_escape_sequence(
                        &format!("\\{}", *next_character as char),
                        SourceOrigin::Plain(span),
                    );
                    return Err(error);
                }
            };
            result.push(escaped_character)
        } else {
            result.push(*character)
        }
    }

    Ok(result)
}
