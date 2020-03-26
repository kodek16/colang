//! Abstract syntax tree representation. This is the interface
//! between the parser (LALRPOP) and the rest of the front-end.

pub type ParseError<'a> =
    lalrpop_util::ParseError<usize, lalrpop_util::lexer::Token<'a>, &'static str>;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    VarDecl(VarDeclStmt),
    Read(ReadStmt),
    Write(WriteStmt),
    If(IfStmt),
    Expr(ExprStmt),
    Block(BlockStmt),
}

#[derive(Debug)]
pub struct VarDeclStmt {
    pub variable_name: String,
    pub variable_type: Option<TypeExpr>,
    pub initializer: Option<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ReadStmt {
    pub variable_name: String,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct WriteStmt {
    pub expression: Expression,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct IfStmt {
    pub cond: Box<Expression>,
    pub then: Box<Statement>,
    pub else_: Option<Box<Statement>>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expression: Expression,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct BlockStmt {
    pub statements: Vec<Statement>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub enum Expression {
    Variable(VariableExpr),
    IntLiteral(IntLiteralExpr),
    BinaryOp(BinaryOperatorExpr),
    If(IfExpr),
    Block(BlockExpr),
}

impl Expression {
    pub fn span(&self) -> InputSpan {
        use Expression::*;
        match self {
            Variable(e) => e.span,
            IntLiteral(e) => e.span,
            BinaryOp(e) => e.span,
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
            Expression::BinaryOp(e) => Expression::BinaryOp(BinaryOperatorExpr {
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
    pub name: String,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct IntLiteralExpr {
    pub value: i32,

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
pub struct IfExpr {
    pub cond: Box<Expression>,
    pub then: Box<Expression>,
    pub else_: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct BlockExpr {
    pub statements: Vec<Statement>,
    pub final_expr: Box<Expression>,

    pub span: InputSpan,
}

#[derive(Debug)]
pub struct TypeExpr {
    pub name: String,

    pub span: InputSpan,
}

#[derive(Copy, Clone, Debug)]
pub struct InputSpan {
    pub start: usize,
    pub end: usize,
}
