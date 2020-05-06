use crate::program::expressions::ExpressionKind;
use crate::program::{Expression, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that constructs an array from a certain number of copies of a base value.
pub struct ArrayFromCopyExpr {
    /// The value of this expression becomes the value of every array element.
    pub element: Box<Expression>,

    /// The value of this expression becomes the array size.
    ///
    /// Must have type `int`. If evaluated to a non-positive value, this causes a runtime error.
    pub size: Box<Expression>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for ArrayFromCopyExpr {
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.array_of(&self.element.type_())
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
