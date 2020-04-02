use crate::ast::InputSpan;
use crate::program::{Expression, ExpressionKind};
use crate::typing::TypeRegistry;

use crate::errors::CompilationError;

#[derive(Debug)]
pub struct IndexExpr {
    collection: Box<Expression>,
    index: Box<Expression>,
}

impl IndexExpr {
    pub fn new(
        collection: Expression,
        index: Expression,
        types: &TypeRegistry,
        span: InputSpan,
    ) -> Result<Expression, CompilationError> {
        let collection_type = &collection.type_;
        let element_type = collection_type.borrow().element_type(types);
        let element_type = match element_type {
            Some(element_type) => element_type,
            None => {
                let error = CompilationError::index_target_not_an_array(
                    collection_type.borrow().name(),
                    span,
                );
                return Err(error);
            }
        };

        let index_type = &index.type_;
        if *index_type != *types.int() {
            let error = CompilationError::array_index_not_int(index_type.borrow().name(), span);
            return Err(error);
        }

        let value_category = collection.value_category;
        let kind = ExpressionKind::Index(IndexExpr {
            collection: Box::new(collection),
            index: Box::new(index),
        });

        Ok(Expression {
            kind,
            type_: element_type,
            value_category,
            span: Some(span),
        })
    }

    pub fn collection(&self) -> &Expression {
        &self.collection
    }

    pub fn index(&self) -> &Expression {
        &self.index
    }
}
