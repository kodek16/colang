//! After all defined functions were analyzed and their bodies filled, this pass looks at all
//! function calls in the program, and instantiates the needed functions for these calls.

use crate::analyzer::visitor::{GlobalVisitor, TypeMemberContext};
use crate::ast::FunctionDef;
use crate::errors;
use crate::program::function::FunctionBody;
use crate::program::visitors::LocalVisitor;
use crate::program::{CallExpr, Function, Program, TypeRegistry};
use crate::source::SourceOrigin;
use crate::CompilerContext;
use std::cell::RefCell;
use std::rc::Rc;

const MAX_FUNCTION_INSTANTIATION_DEPTH: usize = 64;

pub struct FunctionInstantiationsAnalyzerPass;

impl GlobalVisitor for FunctionInstantiationsAnalyzerPass {
    fn revisit_method_def(
        &mut self,
        _: &mut FunctionDef,
        method: Rc<RefCell<Function>>,
        _: &TypeMemberContext,
        context: &mut CompilerContext,
    ) {
        process_function(method, context);
    }

    fn revisit_function_def(
        &mut self,
        _: &mut FunctionDef,
        function: Rc<RefCell<Function>>,
        context: &mut CompilerContext,
    ) {
        process_function(function, context);
    }
}

fn process_function(function: Rc<RefCell<Function>>, context: &mut CompilerContext) {
    match function.borrow().body {
        FunctionBody::Filled(ref body) => {
            let mut call_visitor = CallVisitor::new(context.program.types_mut());
            call_visitor.visit_expression(&mut body.borrow_mut());

            for (call_site, called_function) in &call_visitor.called_functions {
                let result = instantiate_functions_transitively(
                    Rc::clone(&called_function),
                    &mut context.program,
                );

                if let Err(function_chain) = result {
                    let error = errors::function_infinite_dependency_chain(
                        &called_function.borrow(),
                        function_chain,
                        *call_site,
                    );
                    context.errors.push(error);
                }
            }
        }
        FunctionBody::TraitMethod => (),
        _ => panic!(
            "Function `{}` ({:?}) was not filled during the responsible compiler pass",
            function.borrow().name,
            function.borrow().id,
        ),
    }
}

struct CallVisitor<'a> {
    types: &'a mut TypeRegistry,
    called_functions: Vec<(SourceOrigin, Rc<RefCell<Function>>)>,
}

impl<'a> CallVisitor<'a> {
    pub fn new(types: &mut TypeRegistry) -> CallVisitor {
        CallVisitor {
            types,
            called_functions: Vec::new(),
        }
    }
}

impl<'a> LocalVisitor for CallVisitor<'a> {
    fn types(&mut self) -> &mut TypeRegistry {
        self.types
    }

    fn visit_call_expr(&mut self, expression: &mut CallExpr) {
        self.walk(expression);
        self.called_functions
            .push((expression.location, Rc::clone(&expression.function)));
    }
}

fn instantiate_functions_transitively(
    function: Rc<RefCell<Function>>,
    program: &mut Program,
) -> Result<(), Vec<Rc<RefCell<Function>>>> {
    let mut function_stack: Vec<Rc<RefCell<Function>>> = Vec::new();

    fn process(
        function: Rc<RefCell<Function>>,
        stack: &mut Vec<Rc<RefCell<Function>>>,
        program: &mut Program,
    ) -> Result<(), ()> {
        if stack.len() > MAX_FUNCTION_INSTANTIATION_DEPTH {
            return Err(());
        }

        // By calling `add_function` here we make sure that all instantiated functions (including
        // internal method instances) are registered with the program. This makes use of idempotency
        // of `add_function`.
        program.add_function(Rc::clone(&function));

        if function.borrow().body_needs_instantiation() {
            let body = Function::instantiate_body(Rc::clone(&function), program.types_mut());

            let mut call_visitor = CallVisitor::new(program.types_mut());
            call_visitor.visit_expression(&mut body.borrow_mut());
            for (_, called_function) in &call_visitor.called_functions {
                if called_function.borrow().body_needs_instantiation()
                    && !stack.contains(called_function)
                {
                    stack.push(Rc::clone(called_function));
                    process(Rc::clone(called_function), stack, program)?;
                }
            }
        }

        stack.pop();
        Ok(())
    }

    function_stack.push(Rc::clone(&function));
    let result = process(function, &mut function_stack, program);
    result.map_err(|_| function_stack)
}
