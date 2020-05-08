use crate::program::expressions::ExpressionKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{Expression, Field, Instruction, Type, TypeRegistry, ValueCategory};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// An expression that accesses a field of some struct value.
///
/// If `receiver` is an lvalue, this expression is also an lvalue.
pub struct FieldAccessExpr {
    /// The expression evaluating to the struct value that contains the target field.
    pub receiver: Box<Expression>,

    /// The field to be accessed.
    ///
    /// This field must be a member of type of `receiver`.
    pub field: Rc<RefCell<Field>>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for FieldAccessExpr {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        Rc::clone(&self.field.borrow().type_)
    }

    fn value_category(&self) -> ValueCategory {
        self.receiver.value_category()
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for FieldAccessExpr {
    type InstrIter = std::iter::Empty<&'a mut Instruction>;
    type ExprIter = std::iter::Once<&'a mut Expression>;

    fn child_instructions(&'a mut self) -> Self::InstrIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        std::iter::once(&mut self.receiver)
    }
}
