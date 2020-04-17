use crate::ast::InputSpan;
use crate::program::expressions::ExpressionKindImpl;
use crate::program::{Expression, Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub struct ArrayFromCopyExpr {
    pub element: Box<Expression>,
    pub size: Box<Expression>,
    pub span: Option<InputSpan>,
}

impl ExpressionKindImpl for ArrayFromCopyExpr {
    fn calculate_type(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.array_of(&self.element.type_())
    }

    fn calculate_value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn span(&self) -> Option<InputSpan> {
        self.span
    }
}
