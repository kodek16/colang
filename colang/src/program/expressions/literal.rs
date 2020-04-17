use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::{Expression, ExpressionKind, Type, TypeRegistry, ValueCategory};

use crate::program::expressions::ExpressionKindImpl;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub enum LiteralExpr {
    Int(i32, InputSpan),
    Bool(bool, InputSpan),
    Char(u8, InputSpan),
    String(String, InputSpan),
}

impl LiteralExpr {
    pub fn int(value: i32, types: &mut TypeRegistry, span: InputSpan) -> Expression {
        let kind = ExpressionKind::Literal(LiteralExpr::Int(value, span));
        Expression::new(kind, types)
    }

    pub fn bool(value: bool, types: &mut TypeRegistry, span: InputSpan) -> Expression {
        let kind = ExpressionKind::Literal(LiteralExpr::Bool(value, span));
        Expression::new(kind, types)
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
        let kind = ExpressionKind::Literal(LiteralExpr::Char(result, span));
        Ok(Expression::new(kind, types))
    }

    pub fn string(
        value: &str,
        types: &mut TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        let literal = unescape(value, span)?;
        let kind = ExpressionKind::Literal(LiteralExpr::String(literal, span));
        Ok(Expression::new(kind, types))
    }
}

impl ExpressionKindImpl for LiteralExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(match self {
            LiteralExpr::Int(_, _) => types.int(),
            LiteralExpr::Bool(_, _) => types.bool(),
            LiteralExpr::Char(_, _) => types.char(),
            LiteralExpr::String(_, _) => types.string(),
        })
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn span(&self) -> Option<InputSpan> {
        match self {
            LiteralExpr::Int(_, span) => Some(*span),
            LiteralExpr::Bool(_, span) => Some(*span),
            LiteralExpr::Char(_, span) => Some(*span),
            LiteralExpr::String(_, span) => Some(*span),
        }
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
