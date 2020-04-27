//! Type expressions are cleanly distinguished from "usual" expressions: this allows us to
//! have ambiguous syntax like angle brackets vs. comparison operators.

use crate::ast::Identifier;
use crate::source::InputSpan;

#[derive(Debug)]
pub enum TypeExpr {
    Scalar(ScalarTypeExpr),
    Array(ArrayTypeExpr),
    Pointer(PointerTypeExpr),
    TemplateInstance(TemplateInstanceTypeExpr),
}

impl TypeExpr {
    pub fn span(&self) -> InputSpan {
        use TypeExpr::*;
        match self {
            Scalar(type_expr) => type_expr.span,
            Array(type_expr) => type_expr.span,
            Pointer(type_expr) => type_expr.span,
            TemplateInstance(type_expr) => type_expr.span,
        }
    }
}

#[derive(Debug)]
pub struct ScalarTypeExpr {
    pub name: Identifier,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ArrayTypeExpr {
    pub element: Box<TypeExpr>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct PointerTypeExpr {
    pub target: Box<TypeExpr>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct TemplateInstanceTypeExpr {
    pub template: Identifier,
    pub type_arguments: Vec<TypeExpr>,

    pub span: InputSpan,
}
