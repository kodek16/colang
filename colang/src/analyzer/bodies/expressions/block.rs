use super::compile_expression;
use crate::analyzer::bodies::statements::compile_statement;
use crate::context::CompilerContext;
use crate::program::expressions::block::BlockBuilder;
use crate::program::Type;
use crate::{ast, program};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_block_expr(
    block: ast::BlockExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> program::Expression {
    context.scope.push();
    let mut builder = BlockBuilder::new();
    for statement in block.statements {
        compile_statement(statement, &mut builder, context);
    }

    let final_expr = block
        .final_expr
        .map(|expr| compile_expression(*expr, type_hint, context));
    let result = builder.into_expr(final_expr, context.program.types_mut(), block.span);

    context.scope.pop();
    result
}
