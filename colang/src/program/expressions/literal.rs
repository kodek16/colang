use crate::ast::InputSpan;
use crate::program::{Expression, ExpressionKind, TypeRegistry, ValueCategory};

use std::rc::Rc;

#[derive(Debug)]
pub enum LiteralExpr {
    Int(i32),
    Bool(bool),
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
}
