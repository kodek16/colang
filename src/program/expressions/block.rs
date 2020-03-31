use crate::ast::InputSpan;
use crate::program::{
    AllocStmt, DeallocStmt, Expression, ExpressionKind, ReturnStmt, Statement, ValueCategory,
};
use crate::typing::{Type, TypeRegistry};

use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug)]
pub struct BlockExpr {
    statements: Vec<Statement>,
}

impl BlockExpr {
    fn new(
        statements: Vec<Statement>,
        value_type: Rc<RefCell<Type>>,
        span: InputSpan,
    ) -> Expression {
        Expression {
            kind: ExpressionKind::Block(BlockExpr { statements }),
            type_: value_type,
            value_category: ValueCategory::Rvalue,
            span: Some(span),
        }
    }

    pub fn statements(&self) -> impl Iterator<Item = &Statement> {
        self.statements.iter()
    }
}

/// Incremental interface for building block expressions.
pub struct BlockBuilder {
    statements: Vec<Statement>,
}

impl BlockBuilder {
    pub fn new() -> BlockBuilder {
        BlockBuilder { statements: vec![] }
    }

    pub fn append_statement(&mut self, statement: Statement) {
        self.statements.push(statement)
    }

    pub fn into_expr(
        self,
        final_expr: Option<Expression>,
        types: &TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        let mut statements = self.statements;

        let type_ = if let Some(final_expr) = final_expr {
            let type_ = Rc::clone(&final_expr.type_);
            statements.push(ReturnStmt::new(final_expr));
            type_
        } else {
            Rc::clone(types.void())
        };

        // We need to emit a `Dealloc` for every `Alloc` in this block, in reverse order.
        let mut deallocations: Vec<Statement> = statements
            .iter()
            .flat_map(|statement| match statement {
                Statement::Alloc(AllocStmt { variable, .. }) => {
                    Some(DeallocStmt::new(Rc::clone(variable)))
                }
                _ => None,
            })
            .collect();
        deallocations.reverse();
        statements.append(&mut deallocations);

        BlockExpr::new(statements, type_, span)
    }
}
