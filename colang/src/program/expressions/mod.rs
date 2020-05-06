use crate::program::{Type, TypeRegistry, ValueCategory};
use crate::source::{InputSpan, SourceOrigin};
use enum_dispatch::enum_dispatch;
use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub mod address;
pub mod array_from_copy;
pub mod array_from_elements;
pub mod block;
pub mod boolean_op;
pub mod call;
pub mod deref;
pub mod empty;
pub mod error;
pub mod field_access;
pub mod if_;
pub mod is;
pub mod literal;
pub mod new;
pub mod null;
pub mod variable;

pub struct Expression {
    impl_: ExpressionImpl,

    type_: Rc<RefCell<Type>>,
    value_category: ValueCategory,

    dirty: bool,
}

#[enum_dispatch]
pub enum ExpressionImpl {
    Address(address::AddressExpr),
    ArrayFromCopy(array_from_copy::ArrayFromCopyExpr),
    ArrayFromElements(array_from_elements::ArrayFromElementsExpr),
    Block(block::BlockExpr),
    BooleanOp(boolean_op::BooleanOpExpr),
    Call(call::CallExpr),
    Deref(deref::DerefExpr),
    Empty(empty::EmptyExpr),
    Err(error::ErrorExpr),
    FieldAccess(field_access::FieldAccessExpr),
    If(if_::IfExpr),
    Is(is::IsExpr),
    Literal(literal::LiteralExpr),
    New(new::NewExpr),
    Null(null::NullExpr),
    Variable(variable::VariableExpr),
}

#[enum_dispatch(ExpressionImpl)]
pub trait ExpressionKind {
    fn type_(&self, types: &mut TypeRegistry) -> Rc<RefCell<Type>>;
    fn value_category(&self) -> ValueCategory;
    fn location(&self) -> SourceOrigin;
}

impl Deref for Expression {
    type Target = ExpressionImpl;

    fn deref(&self) -> &Self::Target {
        self.panic_if_dirty();
        &self.impl_
    }
}

impl DerefMut for Expression {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.dirty = true;
        &mut self.impl_
    }
}

impl Expression {
    pub fn new(impl_: impl Into<ExpressionImpl>, types: &mut TypeRegistry) -> Expression {
        let impl_ = impl_.into();
        let type_ = impl_.type_(types);
        let value_category = impl_.value_category();

        Expression {
            impl_,
            type_,
            value_category,
            dirty: false,
        }
    }

    pub fn error(span: InputSpan) -> Expression {
        Expression {
            impl_: ExpressionImpl::Err(error::ErrorExpr {
                location: SourceOrigin::Plain(span),
            }),
            type_: Type::error(),
            value_category: ValueCategory::Rvalue,
            dirty: false,
        }
    }

    pub fn type_(&self) -> &Rc<RefCell<Type>> {
        self.panic_if_dirty();
        &self.type_
    }

    pub fn value_category(&self) -> ValueCategory {
        self.panic_if_dirty();
        self.value_category
    }

    pub fn recalculate(&mut self, types: &mut TypeRegistry) {
        self.type_ = self.impl_.type_(types);
        self.value_category = self.impl_.value_category();
        self.dirty = false;
    }

    fn panic_if_dirty(&self) {
        if self.dirty {
            panic!("Expression type was not recalculated after update.")
        }
    }
}

impl ExpressionImpl {
    pub fn is_empty(&self) -> bool {
        match &self {
            ExpressionImpl::Empty(_) => true,
            _ => false,
        }
    }

    pub fn is_error(&self) -> bool {
        match &self {
            ExpressionImpl::Err(_) => true,
            _ => false,
        }
    }
}
