use crate::ast::InputSpan;
use crate::program::{AllocStmt, DeallocStmt, Expression, ExpressionKind, Statement};
use crate::typing::TypeRegistry;

use std::rc::Rc;

#[derive(Debug)]
pub struct BlockExpr {
    statements: Vec<Statement>,
    final_expr: Box<Expression>,
}

impl BlockExpr {
    fn new(
        statements: Vec<Statement>,
        final_expr: Option<Expression>,
        types: &TypeRegistry,
        span: InputSpan,
    ) -> Expression {
        let final_expr = match final_expr {
            Some(final_expr) => Box::new(final_expr),
            None => Box::new(Expression::empty(types)),
        };

        let type_ = Rc::clone(&final_expr.type_);
        let value_category = final_expr.value_category;

        let kind = ExpressionKind::Block(BlockExpr {
            statements,
            final_expr,
        });

        Expression {
            kind,
            type_,
            value_category,
            span: Some(span),
        }
    }

    pub fn statements(&self) -> impl Iterator<Item = &Statement> {
        self.statements.iter()
    }

    pub fn final_expr(&self) -> &Expression {
        &self.final_expr
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

        BlockExpr::new(statements, final_expr, types, span)
    }
}
