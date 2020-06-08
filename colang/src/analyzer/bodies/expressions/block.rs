use super::compile_expression;
use crate::analyzer::bodies::dual::DualNode;
use crate::analyzer::bodies::statements::compile_statement;
use crate::ast;
use crate::context::CompilerContext;
use crate::program::BlockBuilder;
use crate::program::Type;
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_block(
    block: ast::BlockExpr,
    type_hint: Option<Rc<RefCell<Type>>>,
    context: &mut CompilerContext,
) -> DualNode {
    context.scope.push();
    let mut builder = BlockBuilder::new();
    for statement in block.statements {
        compile_statement(statement, &mut builder, context);
    }

    let result = match block.final_expr {
        Some(final_expr) => {
            let final_expr = compile_expression(*final_expr, type_hint, context);
            DualNode::Expression(builder.into_expr(
                final_expr,
                context.program.types_mut(),
                block.span,
            ))
        }
        None => DualNode::Statement(builder.into_stmt(block.span)),
    };

    context.scope.pop();
    result
}
