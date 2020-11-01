use crate::analyzer::bodies::dual::DualNode;
use crate::analyzer::bodies::expressions::compile_expression_or_statement;
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

    let mut items = block.items;
    let result = match items.pop() {
        Some(last) => {
            for item in items {
                compile_statement(item, &mut builder, context);
            }
            match last {
                ast::StmtOrExpr::ExprLike(expr_like) => {
                    match compile_expression_or_statement(expr_like, type_hint, context) {
                        DualNode::Expression(final_expr) => DualNode::Expression(
                            builder.into_expr(final_expr, context.program.types_mut(), block.span),
                        ),
                        DualNode::Statement(final_stmt) => {
                            builder.append_statement(final_stmt);
                            DualNode::Statement(builder.into_stmt(block.span))
                        }
                    }
                }
                _ => {
                    compile_statement(last, &mut builder, context);
                    DualNode::Statement(builder.into_stmt(block.span))
                }
            }
        }
        None => DualNode::Statement(builder.into_stmt(block.span)),
    };

    context.scope.pop();
    result
}
