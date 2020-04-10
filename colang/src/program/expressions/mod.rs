use crate::ast::InputSpan;
use crate::program::{Type, TypeRegistry, ValueCategory};
use std::cell::RefCell;
use std::rc::Rc;

pub mod address;
pub mod array_from_copy;
pub mod array_from_elements;
pub mod block;
pub mod call;
pub mod deref;
pub mod field_access;
pub mod if_;
pub mod literal;
pub mod new;
pub mod variable;

pub struct Expression {
    pub kind: ExpressionKind,
    pub(crate) type_: Rc<RefCell<Type>>,
    pub(crate) value_category: ValueCategory,
    pub(crate) span: Option<InputSpan>,
}

pub enum ExpressionKind {
    Variable(variable::VariableExpr),
    Literal(literal::LiteralExpr),
    Address(address::AddressExpr),
    Deref(deref::DerefExpr),
    New(new::NewExpr),
    ArrayFromElements(array_from_elements::ArrayFromElementsExpr),
    ArrayFromCopy(array_from_copy::ArrayFromCopyExpr),
    FieldAccess(field_access::FieldAccessExpr),
    Call(call::CallExpr),
    If(if_::IfExpr),
    Block(block::BlockExpr),

    /// A no-op expression of type `void`.
    Empty,

    Error,
}

impl Expression {
    pub(crate) fn empty(types: &TypeRegistry) -> Expression {
        Expression {
            kind: ExpressionKind::Empty,
            type_: Rc::clone(types.void()),
            value_category: ValueCategory::Rvalue,
            span: None,
        }
    }

    pub(crate) fn error(span: InputSpan) -> Expression {
        Expression {
            kind: ExpressionKind::Error,
            type_: Type::error(),
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        }
    }

    pub(crate) fn is_error(&self) -> bool {
        match self.kind {
            ExpressionKind::Error => true,
            _ => false,
        }
    }
}
