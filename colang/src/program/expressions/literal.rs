use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory};

use crate::program::expressions::ExpressionKindImpl;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub enum LiteralExpr {
    Int(i32),
    Bool(bool),
    Char(u8),
    String(String),
}

impl LiteralExpr {
    pub fn int(value: i32, types: &mut TypeRegistry, span: InputSpan) -> Expression {
        let kind = ExpressionKind::Literal(LiteralExpr::Int(value));
        Expression::new(kind, Some(span), types)
    }

    pub fn bool(value: bool, types: &mut TypeRegistry, span: InputSpan) -> Expression {
        let kind = ExpressionKind::Literal(LiteralExpr::Bool(value));
        Expression::new(kind, Some(span), types)
    }

    pub fn char(
        value: &str,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        let literal = unescape(value, span)?;
        if literal.len() != 1 {
            let error = CompilationError::char_literal_bad_length(literal.len(), span);
            return Err(error);
        };

        let result = literal.as_bytes()[0];
        let kind = ExpressionKind::Literal(LiteralExpr::Char(result));
        Ok(Expression::new(kind, Some(span), types))
    }

    pub fn string(
        value: &str,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        let literal = unescape(value, span)?;
        let kind = ExpressionKind::Literal(LiteralExpr::String(literal));
        Ok(Expression::new(kind, Some(span), types))
    }
}

impl ExpressionKindImpl for LiteralExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        use LiteralExpr::*;
        Rc::clone(match self {
            Int(_) => types.int(),
            Bool(_) => types.bool(),
            Char(_) => types.char(),
            String(_) => types.string(),
        })
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }
}

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
                _ => {
                    let error = CompilationError::unknown_escape_sequence(
                        &format!("\\{}", *next_character as char),
                        span,
                    );
                    return Err(error);
                }
            };
            result.push(escaped_character)
        } else {
            result.push(*character)
        }
    }

    let result = String::from_utf8(result);
    result.map_err(|_| CompilationError::literal_not_utf8(span))
}
