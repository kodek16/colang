use crate::analyzer::bodies::expressions::compile_expression;
use crate::analyzer::type_exprs;
use crate::context::CompilerContext;
use crate::errors::CompilationError;
use crate::program::expressions::block::BlockBuilder;
use crate::program::{Type, Variable};
use crate::scope::VariableEntity;
use crate::source::SourceOrigin;
use crate::{ast, program};
use std::cell::RefCell;
use std::rc::Rc;

pub fn compile_var_decl_stmt(
    statement: ast::VarDeclStmt,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    for declaration in statement.entries {
        compile_var_decl_entry(declaration, current_block, context);
    }
}

fn compile_var_decl_entry(
    declaration: ast::VarDeclEntry,
    current_block: &mut BlockBuilder,
    context: &mut CompilerContext,
) {
    let name = declaration.variable_name;

    let type_ = declaration
        .variable_type
        .as_ref()
        .map(|type_expr| type_exprs::compile_type_expr_and_ensure_complete(type_expr, context));

    let initializer = declaration
        .initializer
        .map(|initializer| compile_expression(initializer, type_.clone(), context));

    // Possibly infer type from the initializer expression.
    let type_ = match type_ {
        Some(t) => t,
        None => match initializer {
            Some(ref expr) => Rc::clone(expr.type_()),
            None => {
                let error = CompilationError::variable_no_type_or_initializer(
                    &name.text,
                    SourceOrigin::Plain(declaration.span),
                );
                context.errors.push(error);
                Type::error()
            }
        },
    };

    let variable = Rc::new(RefCell::new(Variable::new(
        name.text.clone(),
        type_,
        SourceOrigin::Plain(declaration.span),
        &mut context.program,
    )));

    if let Err(error) = context.scope.add(VariableEntity(Rc::clone(&variable))) {
        context.errors.push(error);
    };

    current_block.add_local_variable(Rc::clone(&variable));

    if let Some(initializer) = initializer {
        if initializer.type_().borrow().is_void() {
            let error = CompilationError::variable_initializer_is_void(&name.text, &initializer);
            context.errors.push(error);
            return;
        }

        if *initializer.type_() != variable.borrow().type_ {
            let error = CompilationError::variable_initializer_type_mismatch(
                &variable.borrow(),
                &initializer,
            );
            context.errors.push(error);
            return;
        }

        current_block.append_instruction(program::AssignInstruction {
            target: program::Expression::new(
                program::VariableExpr {
                    variable,
                    location: SourceOrigin::Plain(name.span),
                },
                context.program.types_mut(),
            ),
            value: initializer,
            location: SourceOrigin::Plain(declaration.span),
        });
    }
}
