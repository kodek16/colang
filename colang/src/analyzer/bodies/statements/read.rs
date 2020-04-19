use crate::analyzer::bodies::expressions::compile_expression;
use crate::errors::CompilationError;
use crate::program::{BlockBuilder, InternalFunctionTag, SourceOrigin, TypeId, ValueCategory};
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_read_stmt(
    statement: ast::ReadStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    for entry in statement.entries {
        compile_read_entry(entry, current_block, context);
    }
}

fn compile_read_entry(
    entry: ast::ReadEntry,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let span = entry.target.span();
    let target = compile_expression(entry.target, None, context);
    if target.is_error() {
        return;
    }

    if target.value_category() != ValueCategory::Lvalue {
        let error = CompilationError::read_target_not_lvalue(target.location());
        context.errors.push(error);
        return;
    }

    let read_function = Rc::clone(match target.type_().borrow().type_id.clone() {
        TypeId::Int => context
            .program
            .internal_function(InternalFunctionTag::ReadInt),
        TypeId::String => context
            .program
            .internal_function(InternalFunctionTag::ReadWord),
        _ => {
            let error = CompilationError::read_unsupported_type(&target);
            context.errors.push(error);
            return;
        }
    });

    let argument = program::Expression::new(
        program::ExpressionKind::Address(program::AddressExpr {
            target: Box::new(target),
            location: SourceOrigin::AddressedForRead(span),
        }),
        context.program.types_mut(),
    );

    let read_call = program::Expression::new(
        program::ExpressionKind::Call(program::CallExpr {
            function: read_function,
            arguments: vec![argument],
            location: SourceOrigin::ReadFunctionCall(span),
        }),
        context.program.types_mut(),
    );

    current_block.append_instruction(program::EvalInstruction::new(read_call));
}
