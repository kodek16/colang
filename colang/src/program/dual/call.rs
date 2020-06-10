use crate::program::expressions::ExpressionKind;
use crate::program::visitors::LocalCodeNode;
use crate::program::{
    Expression, Function, Statement, StatementKind, Type, TypeRegistry, ValueCategory,
};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::rc::Rc;

/// A local code node that executes a function with certain arguments.
///
/// If the executed function is non-void, this node is an expression which takes its value from the
/// value returned from the function. If the executed function is void, this node is a statement.
pub struct Call {
    /// The function to be executed.
    pub function: Rc<RefCell<Function>>,

    /// The expressions that evaluate to arguments that are passed to `function`.
    ///
    /// The number of arguments must match the number of function parameters, and every argument
    /// must have the same type as its corresponding parameter.
    pub arguments: Vec<Expression>,

    /// The location of source code that produced this expression.
    pub location: SourceOrigin,
}

impl ExpressionKind for Call {
    fn type_(&self, _: &mut TypeRegistry) -> Rc<RefCell<Type>> {
        match self.function.borrow().return_type {
            Some(ref return_type) => Rc::clone(return_type),
            None => {
                panic!("Attempt to treat void function call statement as an expression");
            }
        }
    }

    fn value_category(&self) -> ValueCategory {
        ValueCategory::Rvalue
    }

    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl StatementKind for Call {
    fn location(&self) -> SourceOrigin {
        self.location
    }
}

impl<'a> LocalCodeNode<'a> for Call {
    type StmtIter = std::iter::Empty<&'a mut Statement>;
    type ExprIter = std::slice::IterMut<'a, Expression>;

    fn child_statements(&'a mut self) -> Self::StmtIter {
        std::iter::empty()
    }

    fn child_expressions(&'a mut self) -> Self::ExprIter {
        self.arguments.iter_mut()
    }
}
