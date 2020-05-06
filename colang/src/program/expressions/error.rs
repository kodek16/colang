use crate::program::expressions::ExpressionKind;
use crate::program::{Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression signifying a user error.
///
/// This expression cannot be present in a valid program.
pub struct ErrorExpr {
    /// The location of source code that produced an error.
    pub location: SourceOrigin,
}

impl ExpressionKind for ErrorExpr {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Type::error()
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}
