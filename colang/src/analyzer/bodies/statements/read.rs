use crate::analyzer::bodies::expressions::compile_expression;
use crate::errors::CompilationError;
use crate::program::{BlockBuilder, ValueCategory};
use crate::source::SourceOrigin;
use crate::{ast, program, CompilerContext};

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
    let target = compile_expression(entry.target, None, context);
    if target.is_error() {
        return;
    }

    if target.value_category() != ValueCategory::Lvalue {
        let error = CompilationError::read_target_not_lvalue(target.location());
        context.errors.push(error);
        return;
    }

    {
        let target_type = target.type_().borrow();
        if !target_type.is_int() && !target_type.is_string() {
            let error = CompilationError::read_unsupported_type(&target);
            context.errors.push(error);
            return;
        }
    }

    let instruction = program::Instruction::Read(program::ReadInstruction {
        target: target,
        whole_line: false,
        location: SourceOrigin::Plain(entry.span),
    });

    current_block.append_instruction(instruction);
}
