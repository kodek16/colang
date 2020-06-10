use crate::analyzer::bodies::expressions::compile_expression;
use crate::context::CompilerContext;
use crate::program::BlockBuilder;
use crate::source::SourceOrigin;
use crate::{ast, errors, program};
use std::rc::Rc;

pub fn compile_return_stmt(
    statement: ast::ReturnStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let function = Rc::clone(&context.local().function);
    let return_type = Rc::clone(&function.borrow().return_type);

    let expression = statement
        .expression
        .map(|expr| compile_expression(expr, Some(Rc::clone(&return_type)), context));

    if expression.is_some() && function.borrow().is_void() {
        let error =
            errors::return_value_in_void_function(&function.borrow(), expression.as_ref().unwrap());
        context.errors.push(error);
        return;
    }

    if expression.is_none() && !function.borrow().is_void() {
        let error = errors::return_no_value_in_non_void_function(
            &function.borrow(),
            SourceOrigin::Plain(statement.span),
        );
        context.errors.push(error);
        return;
    }

    if let Some(ref expression) = expression {
        if *expression.type_() != return_type {
            let error = errors::return_statement_type_mismatch(&function.borrow(), &expression);
            context.errors.push(error);
            return;
        }
    }

    current_block.append_statement(program::ReturnStmt {
        expression,
        location: SourceOrigin::Plain(statement.span),
    });
}
