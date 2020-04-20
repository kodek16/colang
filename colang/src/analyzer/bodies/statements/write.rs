use crate::analyzer::bodies::expressions::compile_expression;
use crate::errors::CompilationError;
use crate::program::{BlockBuilder, InternalFunctionTag, SourceOrigin, TypeId};
use crate::{ast, program, CompilerContext};
use std::rc::Rc;

pub fn compile_write_stmt(
    statement: ast::WriteStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let expression = compile_expression(statement.expression, None, context);
    if expression.is_error() {
        return;
    }

    let expression_type_id = expression.type_().borrow().type_id.clone();
    let stringified_expr = match expression_type_id {
        TypeId::String => expression,
        TypeId::Int => {
            let location = expression.location();
            let conversion = context
                .program
                .internal_function(InternalFunctionTag::IntToString);

            program::Expression::new(
                program::ExpressionKind::Call(program::CallExpr {
                    function: Rc::clone(conversion),
                    arguments: vec![expression],
                    location: SourceOrigin::Stringified(location.as_plain()),
                }),
                context.program.types_mut(),
            )
        }
        _ => {
            let error = CompilationError::write_value_is_not_stringable(&expression);
            context.errors.push(error);
            return;
        }
    };

    let instruction = program::Instruction::Write(program::WriteInstruction {
        expression: stringified_expr,
        location: SourceOrigin::Plain(statement.span),
    });

    current_block.append_instruction(instruction);

    if statement.newline {
        let newline =
            program::LiteralExpr::string("\n", context.program.types_mut(), statement.span)
                .expect("Couldn't construct '\\n' string literal");
        let instruction = program::Instruction::Write(program::WriteInstruction {
            expression: newline,
            location: SourceOrigin::Plain(statement.span),
        });
        current_block.append_instruction(instruction)
    }
}
