use crate::program::expressions::ExpressionKind;
use crate::program::{Expression, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that constructs an array from a sequence of expressions evaluating to elements.
pub struct ArrayFromElementsExpr {
    /// Expressions that evaluate to array elements.
    ///
    /// All of the expressions must have type `element_type`.
    pub elements: Vec<Expression>,

    /// The type of array elements.
    ///
    /// Also used for inferring the type of empty arrays.
    pub element_type: Rc<RefCell<Type>>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for ArrayFromElementsExpr {
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        types.array_of(&self.element_type)
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
