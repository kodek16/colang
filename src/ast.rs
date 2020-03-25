//! Abstract syntax tree representation. This is the interface
//! between the parser (LALRPOP) and the rest of the front-end.

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    VarDecl(VarDeclStmt),
    Read(ReadStmt),
    Write(WriteStmt),
}

#[derive(Debug)]
pub struct VarDeclStmt {
    pub variable_name: String,
    pub initializer: Option<Expression>,
}

#[derive(Debug)]
pub struct ReadStmt {
    pub variable_name: String,
}

#[derive(Debug)]
pub struct WriteStmt {
    pub expression: Expression,
}

#[derive(Debug)]
pub enum Expression {
    Variable(VariableExpr),
    IntLiteral(IntLiteralExpr),
    Add(AddExpr),
}

#[derive(Debug)]
pub struct VariableExpr(pub String);

#[derive(Debug)]
pub struct IntLiteralExpr(pub i32);

#[derive(Debug)]
pub struct AddExpr(pub Box<Expression>, pub Box<Expression>);

#[derive(Debug)]
pub struct Identifier(pub String);

#[derive(Debug)]
pub struct IntegerLiteral(pub i32);
