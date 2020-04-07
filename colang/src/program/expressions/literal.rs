use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, TypeRegistry, ValueCategory};

use std::rc::Rc;

#[derive(Debug)]
pub enum LiteralExpr {
    Int(i32),
    Bool(bool),
    Char(u8),
}

impl LiteralExpr {
    pub fn int(value: i32, types: &TypeRegistry, span: InputSpan) -> Expression {
        let kind = ExpressionKind::Literal(LiteralExpr::Int(value));
        Expression {
            kind,
            type_: Rc::clone(types.int()),
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        }
    }

    pub fn bool(value: bool, types: &TypeRegistry, span: InputSpan) -> Expression {
        let kind = ExpressionKind::Literal(LiteralExpr::Bool(value));
        Expression {
            kind,
            type_: Rc::clone(types.bool()),
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        }
    }

    pub fn char(
        value: &str,
        types: &TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        let literal = unescape(value, span)?;
        if literal.len() != 1 {
            let error = CompilationError::char_literal_bad_length(literal.len(), span);
            return Err(error);
        };

        let result = literal.as_bytes()[0];
        Ok(Expression {
            kind: ExpressionKind::Literal(LiteralExpr::Char(result)),
            type_: Rc::clone(types.char()),
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        })
    }
}

/// Unescapes a string literal that is presumed to be escaped correctly.
fn unescape(text: &str, span: InputSpan) -> Result<String, CompilationError> {
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
                _ => panic!("Unknown escape sequence: \\{}", *next_character as char),
            };
            result.push(escaped_character)
        } else {
            result.push(*character)
        }
    }

    let result = String::from_utf8(result);
    result.map_err(|_| CompilationError::literal_not_utf8(span))
}
