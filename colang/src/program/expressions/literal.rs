use crate::program::expressions::ExpressionKind;
use crate::program::{Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct LiteralExpr {
    pub value: LiteralValue,
    pub location: SourceOrigin,
}

#[derive(Clone)]
pub enum LiteralValue {
    Int(i32),
    Bool(bool),
    Char(u8),
    String(String),
}

impl ExpressionKind for LiteralExpr {
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(match self.value {
            LiteralValue::Int(_) => types.int(),
            LiteralValue::Bool(_) => types.bool(),
            LiteralValue::Char(_) => types.char(),
            LiteralValue::String(_) => types.string(),
        })
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
