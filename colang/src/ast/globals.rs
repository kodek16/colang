//! AST node definitions for global program structure. Global structure is everything starting
//! from the root `Program` node but stopping at function bodies (not including them).

use crate::ast::expressions::Expression;
use crate::ast::type_expressions::TypeExpr;
use crate::ast::Identifier;
use crate::source::InputSpan;

#[derive(Debug)]
pub struct Program {
    pub structs: Vec<TypeDef>,
    pub traits: Vec<TypeDef>,
    pub functions: Vec<FunctionDef>,
}

impl Program {
    pub fn new() -> Program {
        Program {
            structs: vec![],
            traits: vec![],
            functions: vec![],
        }
    }
}

#[derive(Debug)]
pub struct FunctionDef {
    pub name: Identifier,
    pub parameters: Vec<Parameter>,
    pub return_type: Option<TypeExpr>,
    pub body: Option<Expression>,

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
pub struct TypeDef {
    pub name: Identifier,
    pub type_parameters: Vec<Identifier>,
    pub implemented_traits: Vec<TypeExpr>,
    pub fields: Vec<FieldDef>,
    pub methods: Vec<FunctionDef>,

    pub signature_span: InputSpan,
}

impl TypeDef {
    pub fn new(
        name: Identifier,
        type_parameters: Vec<Identifier>,
        implemented_traits: Vec<TypeExpr>,
        members: Vec<TypeMember>,
        signature_span: InputSpan,
    ) -> TypeDef {
        let mut fields = vec![];
        let mut methods = vec![];

        for member in members {
            match member {
                TypeMember::Field(field_def) => fields.push(field_def),
                TypeMember::Method(method_def) => methods.push(method_def),
            }
        }

        TypeDef {
            name,
            type_parameters,
            implemented_traits,
            fields,
            methods,
            signature_span,
        }
    }
}

// Intermediate type, not present in the final tree.
pub enum TypeMember {
    Field(FieldDef),
    Method(FunctionDef),
}

#[derive(Debug)]
pub struct FieldDef {
    pub name: Identifier,
    pub type_: TypeExpr,

    pub span: InputSpan,
}
