use crate::analyzer::bodies::check_condition_is_bool;
use crate::analyzer::bodies::dual::DualNode;
use crate::analyzer::bodies::expressions::{compile_expression, compile_expression_or_statement};
use crate::context::CompilerContext;
use crate::program::BlockBuilder;
use crate::source::SourceOrigin;
use crate::{ast, program};
use std::rc::Rc;

pub fn compile_while_stmt(
    statement: ast::WhileStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let cond = compile_expression(
        *statement.cond,
        Some(Rc::clone(context.program.types().bool())),
        context,
    );

    let body = compile_expression_or_statement(*statement.body, None, context);
    let body = match body {
        DualNode::Statement(body) => body,
        DualNode::Expression(body) => {
            program::Statement::Eval(program::EvalStmt { expression: body })
        }
    };

    if cond.is_error() {
        return;
    }

    check_condition_is_bool(&cond, context);

    current_block.append_statement(program::WhileStmt {
        cond,
        body: Box::new(body),
        location: SourceOrigin::Plain(statement.span),
    });
}
