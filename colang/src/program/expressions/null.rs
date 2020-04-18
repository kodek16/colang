use crate::ast::InputSpan;
use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub struct NullExpr {
    /// The type that the `null` value supposedly points to.
    pub target_type: Rc<RefCell<Type>>,

    pub span: InputSpan,
}

impl ExpressionKindImpl for NullExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.pointer_to(&self.target_type)
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn span(&self) -> Option<InputSpan> {
        Some(self.span)
    }
}
