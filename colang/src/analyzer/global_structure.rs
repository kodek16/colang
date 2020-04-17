//! In this analysis pass, all "global" information gets analyzed. Simply put, global information
//! is everything except function bodies, that is: fields, function and method signatures.

use crate::analyzer::type_exprs;
use crate::analyzer::utils::global_visitor::GlobalVisitor;
use crate::ast;
use crate::ast::{InputSpan, StructDef};
use crate::errors::CompilationError;
use crate::program::{Function, Type, TypeTemplate, Variable};
use crate::CompilerContext;
use std::cell::RefCell;
use std::rc::Rc;

pub struct GlobalStructureAnalyzerPass {}

impl GlobalStructureAnalyzerPass {
    pub fn new() -> GlobalStructureAnalyzerPass {
        GlobalStructureAnalyzerPass {}
    }
}

impl GlobalVisitor for GlobalStructureAnalyzerPass {
    fn revisit_non_template_struct_def(
        &mut self,
        _: &mut StructDef,
        type_: Rc<RefCell<Type>>,
        context: &mut CompilerContext,
    ) {
        context
            .program
            .types_mut()
            .mark_complete_without_deps(&type_);
    }

    fn revisit_template_struct_def(
        &mut self,
        _: &mut StructDef,
        _: Rc<RefCell<TypeTemplate>>,
        base_type: Rc<RefCell<Type>>,
        context: &mut CompilerContext,
    ) {
        context
            .program
            .types_mut()
            .mark_complete_without_deps(&base_type);
    }

    fn analyze_field_def(
        &mut self,
        field_def: &mut ast::FieldDef,
        current_type: &Rc<RefCell<Type>>,
        context: &mut CompilerContext,
    ) {
        let type_ = type_exprs::compile_type_expr(&field_def.type_, context);

        let field = Variable::new_field(
            field_def.name.text.clone(),
            Rc::clone(&type_),
            Some(field_def.span),
            &mut context.program,
        );
        let field = match field {
            Ok(field) => Rc::new(RefCell::new(field)),
            Err(error) => {
                context.errors.push(error);
                return;
            }
        };
        context
            .defined_fields
            .insert(field_def.span, Rc::clone(&field));

        let result = current_type.borrow_mut().add_field(Rc::clone(&field));
        if let Err(error) = result {
            context.errors.push(error);
        }
    }

    fn analyze_method_def(
        &mut self,
        method_def: &mut ast::FunctionDef,
        current_type: &Rc<RefCell<Type>>,
        context: &mut CompilerContext,
    ) {
        let name = method_def.name.text.clone();
        let return_type = compile_return_type(method_def.return_type.as_ref(), context);

        let method = Rc::new(RefCell::new(Function::new(
            name,
            Rc::clone(&return_type),
            method_def.signature_span,
            context.program.symbol_ids_mut(),
        )));
        context
            .defined_methods
            .insert(method_def.signature_span, Rc::clone(&method));

        context.program.add_function(Rc::clone(&method));
        let result = current_type.borrow_mut().add_method(Rc::clone(&method));
        if let Err(error) = result {
            context.errors.push(error);
        }

        let (self_parameter, normal_parameters): (_, Vec<_>) = match method_def.parameters.get(0) {
            Some(ast::Parameter::Self_(_)) => {
                let mut parameters = method_def.parameters.iter();
                let self_parameter = Some(parameters.next().unwrap().as_self());
                let normal_parameters = parameters.collect();
                (self_parameter, normal_parameters)
            }
            _ => (None, method_def.parameters.iter().collect()),
        };

        // Parameters have their own scope.
        context.scope.push();
        let self_parameter = match self_parameter {
            Some(parameter) => compile_self_parameter(&parameter, Rc::clone(current_type), context),
            None => {
                let error =
                    CompilationError::method_first_parameter_is_not_self(method_def.signature_span);
                context.errors.push(error);

                // For better error recovery.
                // TODO synthesize a more precise span for fake `self`.
                let fake_self = create_variable(
                    "<self>".to_string(),
                    Rc::clone(current_type),
                    Some(method_def.signature_span),
                    context,
                )
                .unwrap();
                fake_self
            }
        };

        let mut normal_parameters: Vec<Rc<RefCell<Variable>>> = normal_parameters
            .iter()
            .flat_map(|parameter| match parameter {
                ast::Parameter::Self_(parameter) => {
                    let error = CompilationError::self_is_not_first_parameter(parameter.span);
                    context.errors.push(error);
                    None
                }
                ast::Parameter::Normal(parameter) => compile_normal_parameter(parameter, context),
            })
            .collect();

        let mut all_parameters = vec![self_parameter];
        all_parameters.append(&mut normal_parameters);
        method.borrow_mut().fill_parameters(all_parameters);

        context.scope.pop();
    }

    fn analyze_function_def(
        &mut self,
        function_def: &mut ast::FunctionDef,
        context: &mut CompilerContext,
    ) {
        let name = function_def.name.text.clone();
        let return_type = compile_return_type(function_def.return_type.as_ref(), context);

        let function = Rc::new(RefCell::new(Function::new(
            name.clone(),
            Rc::clone(&return_type),
            function_def.signature_span,
            context.program.symbol_ids_mut(),
        )));
        context
            .defined_functions
            .insert(function_def.signature_span, Rc::clone(&function));

        context.program.add_function(Rc::clone(&function));
        if let Err(error) = context.scope.add_function(Rc::clone(&function)) {
            context.errors.push(error);
        }

        // Parameters have their own scope.
        context.scope.push();
        let parameters: Vec<Rc<RefCell<Variable>>> = function_def
            .parameters
            .iter()
            .flat_map(|parameter| match parameter {
                ast::Parameter::Self_(parameter) => {
                    let error =
                        CompilationError::self_not_in_method_signature(&name, parameter.span);
                    context.errors.push(error);
                    None
                }
                ast::Parameter::Normal(parameter) => compile_normal_parameter(parameter, context),
            })
            .collect();
        function.borrow_mut().fill_parameters(parameters);

        context.scope.pop();
    }
}

fn compile_return_type(
    return_type: Option<&ast::TypeExpr>,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    match return_type {
        Some(return_type) => type_exprs::compile_type_expr(return_type, context),
        None => Rc::clone(context.program.types().void()),
    }
}

fn compile_normal_parameter(
    parameter: &ast::NormalParameter,
    context: &mut CompilerContext,
) -> Option<Rc<RefCell<Variable>>> {
    let name = parameter.name.text.clone();
    let type_ = type_exprs::compile_type_expr(&parameter.type_, context);

    create_variable(name, type_, Some(parameter.span), context)
}

fn compile_self_parameter(
    parameter: &ast::SelfParameter,
    current_type: Rc<RefCell<Type>>,
    context: &mut CompilerContext,
) -> Rc<RefCell<Variable>> {
    let type_ = match parameter.kind {
        ast::SelfParameterKind::ByValue => current_type,
        ast::SelfParameterKind::ByPointer => context.program.types_mut().pointer_to(&current_type),
    };

    create_variable("<self>".to_string(), type_, Some(parameter.span), context)
        .expect("Couldn't create variable for <self> parameter")
}

fn create_variable(
    name: String,
    type_: Rc<RefCell<Type>>,
    definition_site: Option<InputSpan>,
    context: &mut CompilerContext,
) -> Option<Rc<RefCell<Variable>>> {
    let result = Variable::new_variable(name, type_, definition_site, &mut context.program);
    let variable = match result {
        Ok(variable) => Rc::new(RefCell::new(variable)),
        Err(error) => {
            context.errors.push(error);
            return None;
        }
    };

    if let Err(error) = context.scope.add_variable(Rc::clone(&variable)) {
        context.errors.push(error);
    };
    Some(variable)
}
