//! Abstract syntax tree representation. This is the interface
//! between the parser (LALRPOP) and the rest of the front-end.

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
    pub body: BlockExpr,

    pub signature_span: InputSpan,
}

#[derive(Debug)]
pub struct Parameter {
    pub name: Identifier,
    pub type_: TypeExpr,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct StructDef {
    pub name: Identifier,
    pub fields: Vec<FieldDef>,

    pub signature_span: InputSpan,
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

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub cond: Box<Expression>,
    pub body: Box<BlockExpr>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct AssignStmt {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,

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
    BinaryOp(BinaryOperatorExpr),
    Address(AddressExpr),
    Deref(DerefExpr),
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
            BinaryOp(e) => e.span,
            Address(e) => e.span,
            Deref(e) => e.span,
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

#[derive(Copy, Clone, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Eq,
    NotEq,
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

#[derive(Debug)]
pub enum TypeExpr {
    Scalar(ScalarTypeExpr),
    Array(ArrayTypeExpr),
    Pointer(PointerTypeExpr),
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
pub struct Identifier {
    pub text: String,

    pub span: InputSpan,
}

#[derive(Copy, Clone, Debug)]
pub struct InputSpan {
    pub start: usize,
    pub end: usize,
}

impl InputSpan {
    /// Returns a span for the first character of the file.
    /// Can be useful as a placeholder, when the caller is sure that the span is not going
    /// to be displayed to the end user.
    pub fn top_of_file() -> InputSpan {
        InputSpan { start: 0, end: 1 }
    }
}
