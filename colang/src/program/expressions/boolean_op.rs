use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

pub struct BooleanOpExpr {
    pub op: BooleanOp,
    pub location: SourceOrigin,
}

pub enum BooleanOp {
    And(Box<Expression>, Box<Expression>),
    Or(Box<Expression>, Box<Expression>),
    Not(Box<Expression>),
}

impl ExpressionKindImpl for BooleanOpExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(types.bool())
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
