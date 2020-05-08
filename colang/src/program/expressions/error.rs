use crate::program::expressions::ExpressionKind;
use crate::program::visitors::node::LocalCodeNode;
use crate::program::{Expression, Instruction, Type, TypeRegistry, ValueCategory};
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

impl<'a> LocalCodeNode<'a> for ErrorExpr {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::iter::Empty<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::empty()
    }
}
