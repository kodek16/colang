use crate::program::expressions::ExpressionKind;
use crate::program::visitors::node::LocalCodeNode;
use crate::program::{Expression, Instruction, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// A no-op expression of type `void`.
pub struct EmptyExpr {
    /// The location of source code that produced this expression.
    ///
    /// This is usually an annotated `InputSpan` containing information about why exactly the
    /// expression is empty, and not just `SourceOrigin::Plain`.
    pub location: SourceOrigin,
}

impl ExpressionKind for EmptyExpr {
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(types.void())
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for EmptyExpr {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::iter::Empty<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::empty()
    }
}
