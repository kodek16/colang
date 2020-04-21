//! Abstract syntax tree representation. This is the interface
//! between the parser (LALRPOP) and the rest of the front-end.

use crate::source::InputSpan;
use std::fmt::{self, Display, Formatter};

pub type ParseError<'a> =
    lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'a>, &'static str>;

#[derive(Debug)]
pub struct Program {
    pub structs: Vec<StructDef>,
    pub functions: Vec<FunctionDef>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            structs: vec![],
            functions: vec![],
        }
    }
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<TypeExpr>,
    pub body: Expression,

    pub signature_span: InputSpan,
}

#[derive(Debug)]
pub enum Parameter {
    Normal(NormalParameter),
    Self_(SelfParameter),
}

impl Parameter {
    pub fn as_self(&self) -> &SelfParameter {
        match self {
            Parameter::Self_(parameter) => parameter,
            _ => panic!("Tried to convert a normal parameter into a `self` parameter"),
        }
    }
}

#[derive(Debug)]
pub struct NormalParameter {
    pub name: Identifier,
    pub type_: TypeExpr,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct SelfParameter {
    pub kind: SelfParameterKind,

    pub span: InputSpan,
}

#[derive(Debug)]
pub enum SelfParameterKind {
    ByValue,
    ByPointer,
}

#[derive(Debug)]
pub struct StructDef {
    pub name: Identifier,
    pub type_parameters: Vec<Identifier>,
    pub fields: Vec<FieldDef>,
    pub methods: Vec<FunctionDef>,

    pub signature_span: InputSpan,
}

impl StructDef {
    pub fn new(
        name: Identifier,
        type_parameters: Vec<Identifier>,
        members: Vec<StructMember>,
        signature_span: InputSpan,
    ) -> StructDef {
        let mut fields = vec![];
        let mut methods = vec![];

        for member in members {
            match member {
                StructMember::Field(field_def) => fields.push(field_def),
                StructMember::Method(method_def) => methods.push(method_def),
            }
        }

        StructDef {
            name,
            type_parameters,
            fields,
            methods,
            signature_span,
        }
    }
}

// Intermediate type, not present in the final tree.
pub enum StructMember {
    Field(FieldDef),
    Method(FunctionDef),
}

#[derive(Debug)]
pub struct FieldDef {
    pub name: Identifier,
    pub type_: TypeExpr,

    pub span: InputSpan,
}

#[derive(Debug)]
pub enum Statement {
    VarDecl(VarDeclStmt),
    Read(ReadStmt),
    Write(WriteStmt),
    While(WhileStmt),
    Assign(AssignStmt),
    Return(ReturnStmt),
    Expr(ExprStmt),
}

#[derive(Debug)]
pub struct VarDeclStmt {
    pub entries: Vec<VarDeclEntry>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct VarDeclEntry {
    pub variable_name: Identifier,
    pub variable_type: Option<TypeExpr>,
    pub initializer: Option<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ReadStmt {
    pub entries: Vec<ReadEntry>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ReadEntry {
    pub target: Expression,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct WriteStmt {
    pub expression: Expression,
    pub newline: bool,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub cond: Box<Expression>,
    pub body: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct AssignStmt {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expression: Option<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expression: Expression,

    pub span: InputSpan,
}

#[derive(Debug)]
pub enum Expression {
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

impl Expression {
    pub fn span(&self) -> InputSpan {
        use Expression::*;
        match self {
            Variable(e) => e.span,
            IntLiteral(e) => e.span,
            BoolLiteral(e) => e.span,
            CharLiteral(e) => e.span,
            StringLiteral(e) => e.span,
            Null(e) => e.span,
            Self_(e) => e.span,
            UnaryOp(e) => e.span,
            BinaryOp(e) => e.span,
            Address(e) => e.span,
            Deref(e) => e.span,
            New(e) => e.span,
            Is(e) => e.span,
            ArrayFromElements(e) => e.span,
            ArrayFromCopy(e) => e.span,
            Index(e) => e.span,
            Call(e) => e.span,
            FieldAccess(e) => e.span,
            MethodCall(e) => e.span,
            If(e) => e.span,
            Block(e) => e.span,
        }
    }

    pub fn map_span<F>(self, f: F) -> Expression
    where
        F: FnOnce(&InputSpan) -> InputSpan,
    {
        match self {
            Expression::Variable(e) => Expression::Variable(VariableExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::IntLiteral(e) => Expression::IntLiteral(IntLiteralExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::BoolLiteral(e) => Expression::BoolLiteral(BoolLiteralExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::CharLiteral(e) => Expression::CharLiteral(CharLiteralExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::StringLiteral(e) => Expression::StringLiteral(StringLiteralExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::Null(e) => Expression::Null(NullExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::Self_(e) => Expression::Self_(SelfExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::UnaryOp(e) => Expression::UnaryOp(UnaryOperatorExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::BinaryOp(e) => Expression::BinaryOp(BinaryOperatorExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::Address(e) => Expression::Address(AddressExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::Deref(e) => Expression::Deref(DerefExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::New(e) => Expression::New(NewExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::Is(e) => Expression::Is(IsExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::ArrayFromElements(e) => {
                Expression::ArrayFromElements(ArrayFromElementsExpr {
                    span: f(&e.span),
                    ..e
                })
            }
            Expression::ArrayFromCopy(e) => Expression::ArrayFromCopy(ArrayFromCopyExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::Index(e) => Expression::Index(IndexExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::Call(e) => Expression::Call(CallExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::FieldAccess(e) => Expression::FieldAccess(FieldAccessExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::MethodCall(e) => Expression::MethodCall(MethodCallExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::If(e) => Expression::If(IfExpr {
                span: f(&e.span),
                ..e
            }),
            Expression::Block(e) => Expression::Block(BlockExpr {
                span: f(&e.span),
                ..e
            }),
        }
    }
}

#[derive(Debug)]
pub struct VariableExpr {
    pub name: Identifier,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct IntLiteralExpr {
    pub value: i32,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct BoolLiteralExpr {
    pub value: bool,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct CharLiteralExpr {
    pub value: String,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct StringLiteralExpr {
    pub value: String,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct NullExpr {
    pub span: InputSpan,
}

#[derive(Debug)]
pub struct SelfExpr {
    pub span: InputSpan,
}

#[derive(Debug)]
pub struct UnaryOperatorExpr {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct BinaryOperatorExpr {
    pub operator: BinaryOperator,
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct AddressExpr {
    pub target: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct DerefExpr {
    pub pointer: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct NewExpr {
    pub target_type: TypeExpr,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct IsExpr {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ArrayFromElementsExpr {
    pub elements: Vec<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ArrayFromCopyExpr {
    pub element: Box<Expression>,
    pub size: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct IndexExpr {
    pub collection: Box<Expression>,
    pub index: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct FieldAccessExpr {
    pub receiver: Box<Expression>,
    pub field: Identifier,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct CallExpr {
    pub function_name: Identifier,
    pub arguments: Vec<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct MethodCallExpr {
    pub receiver: Box<Expression>,
    pub method: Identifier,
    pub arguments: Vec<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct IfExpr {
    pub cond: Box<Expression>,
    pub then: Box<Expression>,
    pub else_: Option<Box<Expression>>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct BlockExpr {
    pub statements: Vec<Statement>,
    pub final_expr: Option<Box<Expression>>,

    pub span: InputSpan,
}

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

#[derive(Debug)]
pub struct Identifier {
    pub text: String,

    pub span: InputSpan,
}
