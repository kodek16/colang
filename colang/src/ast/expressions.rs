//! Definitions of "expression-like" code nodes.
//!
//! Also known as "syntactic expressions", these are nodes that are likely to be determined to be
//! expressions after analysis.

use crate::ast::statements::StmtOrExpr;
use crate::ast::type_expressions::TypeExpr;
use crate::ast::Identifier;
use crate::source::InputSpan;
use enum_dispatch::enum_dispatch;
use std::fmt::{self, Display, Formatter};

/// Expression-like code node that can turn out to be either statement or expression.
///
/// See `colang::ast::statements` for more context on statements and expressions. The reason why
/// these specific nodes are grouped in a separate variant type is mostly historical.
#[enum_dispatch]
#[derive(Debug)]
pub enum ExpressionLike {
    Variable(VariableExpr),
    IntLiteral(IntLiteralExpr),
    BoolLiteral(BoolLiteralExpr),
    CharLiteral(CharLiteralExpr),
    StringLiteral(StringLiteralExpr),
    Null(NullExpr),
    Self_(SelfExpr),
    UnaryOp(UnaryOperatorExpr),
    BinaryOp(BinaryOperatorExpr),
    Address(AddressExpr),
    Deref(DerefExpr),
    New(NewExpr),
    Is(IsExpr),
    ArrayFromElements(ArrayFromElementsExpr),
    ArrayFromCopy(ArrayFromCopyExpr),
    Index(IndexExpr),
    Call(CallExpr),
    FieldAccess(FieldAccessExpr),
    MethodCall(MethodCallExpr),
    If(IfExpr),
    Block(BlockExpr),
}

// We don't expose the generated methods of `Expression` to the outside, keeping usage of
// `enum_dispatch` as opaque as possible.
#[enum_dispatch(ExpressionLike)]
trait ExpressionKind {
    fn span_(&self) -> InputSpan;
    fn span_mut_(&mut self) -> &mut InputSpan;
}

// It would be nice to provide a #[derive(...)] macro, but this would require creating (and
// potentially publishing) a separate crate just for the sake of this macro, which we don't
// really want to do.
macro_rules! impl_expr_kind {
    ($type_name:ty) => {
        impl ExpressionKind for $type_name {
            fn span_(&self) -> InputSpan {
                self.span
            }

            fn span_mut_(&mut self) -> &mut InputSpan {
                &mut self.span
            }
        }
    };
}

impl ExpressionLike {
    pub fn span(&self) -> InputSpan {
        self.span_()
    }

    pub fn map_span(mut self, f: impl FnOnce(InputSpan) -> InputSpan) -> ExpressionLike {
        *self.span_mut_() = f(self.span_());
        self
    }
}

#[derive(Debug)]
pub struct VariableExpr {
    pub name: Identifier,

    pub span: InputSpan,
}
impl_expr_kind!(VariableExpr);

#[derive(Debug)]
pub struct IntLiteralExpr {
    pub value: i32,

    pub span: InputSpan,
}
impl_expr_kind!(IntLiteralExpr);

#[derive(Debug)]
pub struct BoolLiteralExpr {
    pub value: bool,

    pub span: InputSpan,
}
impl_expr_kind!(BoolLiteralExpr);

#[derive(Debug)]
pub struct CharLiteralExpr {
    pub value: String,

    pub span: InputSpan,
}
impl_expr_kind!(CharLiteralExpr);

#[derive(Debug)]
pub struct StringLiteralExpr {
    pub value: String,

    pub span: InputSpan,
}
impl_expr_kind!(StringLiteralExpr);

#[derive(Debug)]
pub struct NullExpr {
    pub span: InputSpan,
}
impl_expr_kind!(NullExpr);

#[derive(Debug)]
pub struct SelfExpr {
    pub span: InputSpan,
}
impl_expr_kind!(SelfExpr);

#[derive(Debug)]
pub struct UnaryOperatorExpr {
    pub operator: UnaryOperator,
    pub operand: Box<ExpressionLike>,

    pub span: InputSpan,
}
impl_expr_kind!(UnaryOperatorExpr);

#[derive(Debug)]
pub struct BinaryOperatorExpr {
    pub operator: BinaryOperator,
    pub lhs: Box<ExpressionLike>,
    pub rhs: Box<ExpressionLike>,

    pub span: InputSpan,
}
impl_expr_kind!(BinaryOperatorExpr);

#[derive(Debug)]
pub struct AddressExpr {
    pub target: Box<ExpressionLike>,

    pub span: InputSpan,
}
impl_expr_kind!(AddressExpr);

#[derive(Debug)]
pub struct DerefExpr {
    pub pointer: Box<ExpressionLike>,

    pub span: InputSpan,
}
impl_expr_kind!(DerefExpr);

#[derive(Debug)]
pub struct NewExpr {
    pub target_type: TypeExpr,

    pub span: InputSpan,
}
impl_expr_kind!(NewExpr);

#[derive(Debug)]
pub struct IsExpr {
    pub lhs: Box<ExpressionLike>,
    pub rhs: Box<ExpressionLike>,

    pub span: InputSpan,
}
impl_expr_kind!(IsExpr);

#[derive(Debug)]
pub struct ArrayFromElementsExpr {
    pub elements: Vec<ExpressionLike>,

    pub span: InputSpan,
}
impl_expr_kind!(ArrayFromElementsExpr);

#[derive(Debug)]
pub struct ArrayFromCopyExpr {
    pub element: Box<ExpressionLike>,
    pub size: Box<ExpressionLike>,

    pub span: InputSpan,
}
impl_expr_kind!(ArrayFromCopyExpr);

#[derive(Debug)]
pub struct IndexExpr {
    pub collection: Box<ExpressionLike>,
    pub index: Box<ExpressionLike>,

    pub span: InputSpan,
}
impl_expr_kind!(IndexExpr);

#[derive(Debug)]
pub struct FieldAccessExpr {
    pub receiver: Box<ExpressionLike>,
    pub field: Identifier,

    pub span: InputSpan,
}
impl_expr_kind!(FieldAccessExpr);

#[derive(Debug)]
pub struct CallExpr {
    pub function_name: Identifier,
    pub arguments: Vec<ExpressionLike>,

    pub span: InputSpan,
}
impl_expr_kind!(CallExpr);

#[derive(Debug)]
pub struct MethodCallExpr {
    pub receiver: Box<ExpressionLike>,
    pub method: Identifier,
    pub arguments: Vec<ExpressionLike>,

    pub span: InputSpan,
}
impl_expr_kind!(MethodCallExpr);

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Box<ExpressionLike>,
    pub then: Box<ExpressionLike>,
    pub else_: Option<Box<ExpressionLike>>,

    pub span: InputSpan,
}
impl_expr_kind!(IfExpr);

#[derive(Debug)]
pub struct BlockExpr {
    pub statements: Vec<StmtOrExpr>,
    pub final_expr: Option<Box<ExpressionLike>>,

    pub span: InputSpan,
}
impl_expr_kind!(BlockExpr);

#[derive(Copy, Clone, Debug)]
pub enum UnaryOperator {
    LogicalNot,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            UnaryOperator::LogicalNot => "!",
        };
        write!(f, "{}", symbol)
    }
}

#[derive(Copy, Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Eq,
    NotEq,
    LogicalAnd,
    LogicalOr,
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use BinaryOperator::*;
        let symbol = match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            Less => "<",
            Greater => ">",
            LessEq => "<=",
            GreaterEq => ">=",
            Eq => "==",
            NotEq => "!=",
            LogicalAnd => "&&",
            LogicalOr => "||",
        };
        write!(f, "{}", symbol)
    }
}
