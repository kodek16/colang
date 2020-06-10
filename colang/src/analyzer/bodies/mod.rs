//! This analyzer pass is responsible for analyzing all function bodies defined in the program.

mod dual;
mod expressions;
mod statements;

use crate::analyzer::bodies::dual::DualNode;
use crate::analyzer::bodies::expressions::{compile_expression, compile_expression_or_statement};
use crate::analyzer::visitor::{GlobalVisitor, TypeMemberContext};
use crate::errors;
use crate::program::function::FunctionBody;
use crate::program::{EvalStmt, ExpressionKind, Function, Statement, Variable};
use crate::program::{ReturnStmt, StatementKind};
use crate::scope::VariableEntity;
use crate::source::{InputSpan, SourceOrigin};
use crate::{ast, program, CompilerContext};
use std::cell::RefCell;
use std::iter;
use std::rc::Rc;

pub struct BodiesAnalyzerPass;

impl GlobalVisitor for BodiesAnalyzerPass {
    fn revisit_method_def(
        &mut self,
        method_def: &mut ast::FunctionDef,
        method: Rc<RefCell<Function>>,
        type_context: &TypeMemberContext,
        context: &mut CompilerContext,
    ) {
        match type_context {
            TypeMemberContext::TraitSelfType { trait_, .. } => {
                if method_def.body.is_some() {
                    let error = errors::method_with_body_in_trait(method_def, &trait_.borrow());
                    context.errors.push(error);
                }

                method.borrow_mut().body = FunctionBody::TraitMethod;
            }
            TypeMemberContext::Type(_) | TypeMemberContext::TemplateBaseType { .. } => {
                context.push_local(
                    Rc::clone(&method),
                    Some(Rc::clone(&method.borrow().parameters.get(0).expect(
                        &format!(
                            "Attempt to parse method `{}` of type `{}` which is in an error state: no `self`",
                            method_def.name.text,
                            type_context.type_().borrow().name
                        ),
                    ))),
                );

                // Parameters have their own scope.
                context.scope.push();
                for parameter in &method.borrow().parameters[1..] {
                    // Ignore errors, they should be already reported in the previous phase.
                    let _ = context.scope.add(VariableEntity(Rc::clone(&parameter)));
                }

                parse_and_fill_function_body_if_present(method, method_def, context);

                context.scope.pop();
                context.pop_local();
            }
        }
    }

    fn revisit_function_def(
        &mut self,
        function_def: &mut ast::FunctionDef,
        function: Rc<RefCell<Function>>,
        context: &mut CompilerContext,
    ) {
        context.push_local(Rc::clone(&function), None);

        // Parameters have their own scope.
        context.scope.push();

        for parameter in &function.borrow().parameters {
            // Ignore errors, they should be already reported in the previous phase.
            let _ = context.scope.add(VariableEntity(Rc::clone(&parameter)));
        }

        parse_and_fill_function_body_if_present(function, function_def, context);

        context.scope.pop();
        context.pop_local();
    }
}

fn parse_and_fill_function_body_if_present(
    function: Rc<RefCell<Function>>,
    definition: &mut ast::FunctionDef,
    context: &mut CompilerContext,
) {
    // Extract body from definition.
    let mut body = None;
    std::mem::swap(&mut body, &mut definition.body);

    if let Some(body) = body {
        let type_hint = function.borrow().return_type.as_ref().map(Rc::clone);
        let body = compile_expression_or_statement(body, type_hint, context);

        let body = match body {
            DualNode::Statement(body) => {
                if function.borrow().is_void() {
                    body
                } else {
                    let error =
                        errors::non_void_function_body_is_statement(&function.borrow(), &body);
                    context.errors.push(error);
                    Statement::error(body.location().as_plain())
                }
            }
            DualNode::Expression(body) => match function.borrow().return_type {
                Some(ref return_type) => {
                    let body_type = body.type_();

                    if !body_type.borrow().is_error()
                        && !return_type.borrow().is_error()
                        && body_type != return_type
                    {
                        let error = errors::function_body_type_mismatch(&function.borrow(), &body);
                        context.errors.push(error);
                    }

                    Statement::Return(ReturnStmt {
                        location: body.location(),
                        expression: Some(body),
                    })
                }
                None => Statement::Eval(EvalStmt { expression: body }),
            },
        };

        function.borrow_mut().body = FunctionBody::Filled(Rc::new(RefCell::new(body)));
    } else {
        let error = errors::function_body_missing(definition);
        context.errors.push(error);

        function.borrow_mut().body = FunctionBody::Filled(Rc::new(RefCell::new(Statement::error(
            definition.signature_span,
        ))));
    }
}

/// Automatic pointer dereferencing: in some contexts where it's obvious that pointers
/// have to be dereferenced the user can omit the dereference operator.
fn maybe_deref(
    expression: program::Expression,
    context: &mut CompilerContext,
) -> program::Expression {
    let location = expression.location();

    if expression.type_().borrow().is_pointer() {
        program::Expression::new(
            program::DerefExpr {
                pointer: Box::new(expression),
                location: SourceOrigin::AutoDeref(location.as_plain()),
            },
            context.program.types_mut(),
        )
    } else {
        expression
    }
}

/// Checks that a condition expression has type `bool`.
fn check_condition_is_bool(condition: &program::Expression, context: &mut CompilerContext) {
    let cond_type = condition.type_();
    if !cond_type.borrow().is_bool() {
        let error = errors::condition_not_bool(&cond_type.borrow(), condition.location());
        context.errors.push(error);
    }
}

/// Checks that argument number and types conform to function signature.
fn check_argument_types(
    function: &Rc<RefCell<Function>>,
    arguments: &[program::Expression],
    span: InputSpan,
    context: &mut CompilerContext,
) -> Result<(), ()> {
    let function = function.borrow();
    if function.parameters.len() != arguments.len() {
        let error = errors::call_wrong_number_of_arguments(
            &function,
            arguments.len(),
            SourceOrigin::Plain(span),
        );
        context.errors.push(error);
        return Err(());
    }

    let parameters = function.parameters.iter();

    let mut had_errors = false;
    for (argument, parameter) in arguments.iter().zip(parameters) {
        if *argument.type_() != *parameter.borrow().type_
            && !argument.type_().borrow().is_error()
            && !parameter.borrow().type_.borrow().is_error()
        {
            let error = errors::call_argument_type_mismatch(&argument, &parameter.borrow());
            context.errors.push(error);
            had_errors = true;
        }
    }

    if !had_errors {
        Ok(())
    } else {
        Err(())
    }
}

/// Compile a list of arguments using type hints from their corresponding parameters.
fn compile_arguments<'a>(
    arguments: impl Iterator<Item = ast::Expression>,
    parameters: impl Iterator<Item = &'a Rc<RefCell<Variable>>>,
    context: &mut CompilerContext,
) -> Vec<program::Expression> {
    let hints = parameters
        .map(|parameter| Some(Rc::clone(&parameter.borrow().type_)))
        .chain(iter::repeat(None));
    arguments
        .zip(hints)
        .map(|(argument, hint)| compile_expression(argument, hint, context))
        .collect()
}
