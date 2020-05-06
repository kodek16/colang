use crate::analyzer::bodies::expressions::compile_expression;
use crate::context::CompilerContext;
use crate::errors::CompilationError;
use crate::program::expressions::block::BlockBuilder;
use crate::program::{ExpressionKind, ValueCategory};
use crate::source::SourceOrigin;
use crate::{ast, program};
use std::rc::Rc;

pub fn compile_assign_stmt(
    statement: ast::AssignStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let lhs = compile_expression(*statement.lhs, None, context);
    let rhs = compile_expression(*statement.rhs, Some(Rc::clone(lhs.type_())), context);
    if lhs.is_error() || rhs.is_error() {
        return;
    }

    if lhs.value_category() != ValueCategory::Lvalue {
        let error = CompilationError::assignment_target_not_lvalue(lhs.location());
        context.errors.push(error);
        return;
    }

    let lhs_type = lhs.type_();
    let rhs_type = rhs.type_();

    if lhs_type != rhs_type {
        let error = CompilationError::assignment_type_mismatch(
            &lhs,
            &rhs,
            SourceOrigin::Plain(statement.span),
        );
        context.errors.push(error);
        return;
    }

    current_block.append_instruction(program::AssignInstruction {
        target: lhs,
        value: rhs,
        location: SourceOrigin::Plain(statement.span),
    });
}
