//! In this pass, all globally referenced types are ensured to be fully complete.
//!
//! This may involve instantiating the types of fields or function signatures.

use crate::analyzer::visitor::{GlobalVisitor, TypeMemberContext};
use crate::ast::{self, FieldDef, FunctionDef};
use crate::errors;
use crate::program::{Field, Function, Type};
use crate::source::{InputSpan, SourceOrigin};
use crate::CompilerContext;
use std::cell::RefCell;
use std::rc::Rc;

pub struct CompleteTypesAnalyzerPass;

impl GlobalVisitor for CompleteTypesAnalyzerPass {
    fn revisit_field_def(
        &mut self,
        field_def: &mut FieldDef,
        field: Rc<RefCell<Field>>,
        _: &TypeMemberContext,
        context: &mut CompilerContext,
    ) {
        let field_type = Rc::clone(&field.borrow().type_);
        complete_type(field_type, SourceOrigin::Plain(field_def.span), context);
    }

    fn revisit_method_def(
        &mut self,
        method_def: &mut FunctionDef,
        method: Rc<RefCell<Function>>,
        _: &TypeMemberContext,
        context: &mut CompilerContext,
    ) {
        complete_function_types(method_def, method, context);
    }

    fn revisit_function_def(
        &mut self,
        function_def: &mut FunctionDef,
        function: Rc<RefCell<Function>>,
        context: &mut CompilerContext,
    ) {
        complete_function_types(function_def, function, context);
    }
}

fn complete_function_types(
    function_def: &mut ast::FunctionDef,
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
            .map(|type_expr| SourceOrigin::Plain(type_expr.span()))
            // Completing `void` should never fail.
            .unwrap_or(SourceOrigin::Plain(InputSpan::top_of_file())),
        context,
    );
}

fn complete_type(
    type_: Rc<RefCell<Type>>,
    reference_location: SourceOrigin,
    context: &mut CompilerContext,
) {
    let result = Type::ensure_is_fully_complete(Rc::clone(&type_), context.program.types_mut());
    if let Err(type_chain) = result {
        let error =
            errors::type_infinite_dependency_chain(&type_.borrow(), type_chain, reference_location);
        context.errors.push(error);
    }
}
