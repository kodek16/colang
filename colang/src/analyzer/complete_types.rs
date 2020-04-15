//! In this pass, all globally referenced types are ensured to be fully complete.

use crate::analyzer::utils::global_visitor::GlobalVisitor;
use crate::ast::{self, FieldDef, FunctionDef, InputSpan, StructDef};
use crate::errors::CompilationError;
use crate::program::{Function, Type, TypeTemplate, Variable};
use crate::CompilerContext;
use std::cell::RefCell;
use std::rc::Rc;

pub struct CompleteTypesAnalyzerPass {}

impl CompleteTypesAnalyzerPass {
    pub fn new() -> CompleteTypesAnalyzerPass {
        CompleteTypesAnalyzerPass {}
    }
}

impl GlobalVisitor for CompleteTypesAnalyzerPass {
    fn revisit_non_template_struct_def(
        &mut self,
        _: &StructDef,
        _: Rc<RefCell<Type>>,
        _: &mut CompilerContext,
    ) {
        // no-op
    }

    fn revisit_template_struct_def(
        &mut self,
        _: &StructDef,
        _: Rc<RefCell<TypeTemplate>>,
        _: Rc<RefCell<Type>>,
        _: &mut CompilerContext,
    ) {
        // no-op
    }

    fn revisit_field_def(
        &mut self,
        field_def: &FieldDef,
        _: &Rc<RefCell<Type>>,
        field: Rc<RefCell<Variable>>,
        context: &mut CompilerContext,
    ) {
        let field_type = Rc::clone(&field.borrow().type_);
        complete_type(field_type, field_def.span, context);
    }

    fn revisit_method_def(
        &mut self,
        method_def: &FunctionDef,
        _: &Rc<RefCell<Type>>,
        method: Rc<RefCell<Function>>,
        context: &mut CompilerContext,
    ) {
        complete_function_types(method_def, method, context);
    }

    fn revisit_function_def(
        &mut self,
        function_def: &FunctionDef,
        function: Rc<RefCell<Function>>,
        context: &mut CompilerContext,
    ) {
        complete_function_types(function_def, function, context);
    }
}

fn complete_function_types(
    function_def: &ast::FunctionDef,
    function: Rc<RefCell<Function>>,
    context: &mut CompilerContext,
) {
    for parameter in &function.borrow().parameters {
        let parameter_type = Rc::clone(&parameter.borrow().type_);
        complete_type(
            parameter_type,
            parameter.borrow().definition_site.expect(&format!(
                "Parameter `{}` of function `{}` does not refer to a source location",
                parameter.borrow().name,
                function.borrow().name,
            )),
            context,
        );
    }

    let return_type = Rc::clone(&function.borrow().return_type);
    complete_type(
        return_type,
        function_def
            .return_type
            .as_ref()
            .map(|type_expr| type_expr.span())
            // Completing `void` should never fail.
            .unwrap_or(InputSpan::top_of_file()),
        context,
    );
}

fn complete_type(
    type_: Rc<RefCell<Type>>,
    reference_location: InputSpan,
    context: &mut CompilerContext,
) {
    let result =
        Type::ensure_is_complete_with_dependencies(Rc::clone(&type_), context.program.types_mut());
    if let Err(type_chain) = result {
        let type_chain: Vec<_> = type_chain
            .iter()
            .map(|type_| type_.borrow().name().to_string())
            .collect();

        let error = CompilationError::type_infinite_dependency_chain(
            type_.borrow().name(),
            type_chain.iter().map(AsRef::as_ref).collect(),
            reference_location,
        );
        context.errors.push(error);
    }
}
