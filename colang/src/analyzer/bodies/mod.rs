//! This analyzer pass is responsible for analyzing all function bodies defined in the program.

mod expressions;
mod statements;

use crate::analyzer::bodies::expressions::compile_expression;
use crate::analyzer::utils::global_visitor::GlobalVisitor;
use crate::ast::{FunctionDef, InputSpan};
use crate::errors::CompilationError;
use crate::program::{Function, FunctionBody, Type, Variable};
use crate::{ast, program, CompilerContext};
use std::cell::RefCell;
use std::iter;
use std::rc::Rc;

pub struct BodiesAnalyzerPass {}

impl BodiesAnalyzerPass {
    pub fn new() -> BodiesAnalyzerPass {
        BodiesAnalyzerPass {}
    }
}

impl GlobalVisitor for BodiesAnalyzerPass {
    fn revisit_method_def(
        &mut self,
        method_def: &mut FunctionDef,
        current_type: &Rc<RefCell<Type>>,
        method: Rc<RefCell<Function>>,
        context: &mut CompilerContext,
    ) {
        // Parameters have their own scope.
        context.scope.push();
        context.self_ = Some(Rc::clone(&method.borrow().parameters.get(0).expect(
            &format!(
                "Attempt to parse method `{}` of type `{}` which is in an error state: no `self` parameter",
                method_def.name.text,
                current_type.borrow().name()
            ),
        )));
        for parameter in &method.borrow().parameters[1..] {
            // Ignore errors, they should be already reported in the previous phase.
            let _ = context.scope.add_variable(Rc::clone(&parameter));
        }

        // Extract body from method definition.
        let mut method_body = ast::Expression::Block(ast::BlockExpr {
            statements: Vec::new(),
            final_expr: None,
            span: method_def.body.span(),
        });
        std::mem::swap(&mut method_body, &mut method_def.body);

        let body = compile_expression(
            method_body,
            Some(Rc::clone(&method.borrow().return_type)),
            context,
        );

        context.self_ = None;
        context.scope.pop();

        fill_function_body(method, body, context);
    }

    fn revisit_function_def(
        &mut self,
        function_def: &mut FunctionDef,
        function: Rc<RefCell<Function>>,
        context: &mut CompilerContext,
    ) {
        // Parameters have their own scope.
        context.scope.push();

        for parameter in &function.borrow().parameters {
            // Ignore errors, they should be already reported in the previous phase.
            let _ = context.scope.add_variable(Rc::clone(&parameter));
        }

        // Extract body from function definition.
        let mut function_body = ast::Expression::Block(ast::BlockExpr {
            statements: Vec::new(),
            final_expr: None,
            span: function_def.body.span(),
        });
        std::mem::swap(&mut function_body, &mut function_def.body);

        let body = compile_expression(
            function_body,
            Some(Rc::clone(&function.borrow().return_type)),
            context,
        );

        context.scope.pop();

        fill_function_body(function, body, context);
    }
}

fn fill_function_body(
    function: Rc<RefCell<Function>>,
    body: program::Expression,
    context: &mut CompilerContext,
) {
    {
        let body_type = body.type_();
        let return_type = &function.borrow().return_type;

        if body_type != return_type {
            let error = CompilationError::function_body_type_mismatch(
                return_type.borrow().name(),
                body_type.borrow().name(),
                function
                    .borrow()
                    .definition_site
                    .expect("Attempt to fill body for internal function"),
            );
            context.errors.push(error);
        }
    }

    function.borrow_mut().body = FunctionBody::Filled(Rc::new(RefCell::new(body)));
}

/// Automatic pointer dereferencing: in some contexts where it's obvious that pointers
/// have to be dereferenced the user can omit the dereference operator.
fn maybe_deref(
    expression: program::Expression,
    context: &mut CompilerContext,
) -> program::Expression {
    let span = expression.span();
    if expression.type_().borrow().is_pointer() {
        program::DerefExpr::new(expression, context.program.types_mut(), span).unwrap()
    } else {
        expression
    }
}

/// Construct a Call expression while also taking a note that the body of the called function
/// must be analyzed or instantiated at some point.
fn call_and_remember(
    function: Rc<RefCell<Function>>,
    arguments: Vec<program::Expression>,
    span: InputSpan,
    context: &mut CompilerContext,
) -> Result<program::Expression, CompilationError> {
    match function.borrow().body {
        FunctionBody::ToBeFilled | FunctionBody::ToBeInstantiated { .. } => {
            context
                .incomplete_functions
                .insert(function.borrow().id.clone(), Rc::clone(&function));
        }
        _ => (),
    }
    program::CallExpr::new(function, arguments, context.program.types_mut(), span)
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
