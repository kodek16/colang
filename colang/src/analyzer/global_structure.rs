//! In this analysis pass, all "global" information gets analyzed. Simply put, global information
//! is everything except function bodies, that is: fields, function and method signatures.

use crate::analyzer::type_exprs;
use crate::analyzer::visitor::{GlobalVisitor, TypeMemberContext};
use crate::program::{Field, Function, Type, TypeRef, Variable};
use crate::scope::{FunctionEntity, VariableEntity};
use crate::source::SourceOrigin;
use crate::CompilerContext;
use crate::{ast, errors};
use std::cell::RefCell;
use std::rc::Rc;

pub struct GlobalStructureAnalyzerPass;

impl GlobalVisitor for GlobalStructureAnalyzerPass {
    fn analyze_field_def(
        &mut self,
        field_def: &mut ast::FieldDef,
        type_context: &TypeMemberContext,
        context: &mut CompilerContext,
    ) {
        match type_context {
            TypeMemberContext::TraitSelfType { trait_, .. } => {
                let error = errors::field_in_trait(field_def, &trait_.borrow());
                context.errors.push(error);

                // Create a stub to keep `globals` in order.
                let field_stub = Rc::new(RefCell::new(Field::new(
                    field_def.name.text.clone(),
                    Type::error(),
                    SourceOrigin::Plain(field_def.span),
                    &mut context.program,
                )));
                context.globals.register_field(field_def, field_stub);
            }
            TypeMemberContext::Type(_) | TypeMemberContext::TemplateBaseType { .. } => {
                let type_ = type_exprs::compile_type_expr(&field_def.type_, context);

                let field = Rc::new(RefCell::new(Field::new(
                    field_def.name.text.clone(),
                    Rc::clone(&type_),
                    SourceOrigin::Plain(field_def.span),
                    &mut context.program,
                )));
                context
                    .globals
                    .register_field(&field_def, Rc::clone(&field));

                let result = type_context
                    .type_()
                    .borrow_mut()
                    .add_field(Rc::clone(&field));
                if let Err(error) = result {
                    let error = error.into_direct_add_error(field.borrow().definition_site);
                    context.errors.push(error);
                }
            }
        }
    }

    fn analyze_method_def(
        &mut self,
        method_def: &mut ast::FunctionDef,
        type_context: &TypeMemberContext,
        context: &mut CompilerContext,
    ) {
        let current_type = type_context.type_();

        let name = method_def.name.text.clone();
        let return_type = compile_return_type(method_def.return_type.as_ref(), context);

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
                let error = errors::method_first_parameter_is_not_self(SourceOrigin::Plain(
                    method_def.signature_span,
                ));
                context.errors.push(error);

                // For better error recovery.
                // TODO(#2) synthesize a more precise span for fake `self`.
                let fake_self = create_parameter(
                    "<self>".to_string(),
                    TypeRef::new(Rc::clone(current_type), None),
                    SourceOrigin::Plain(method_def.signature_span),
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
                    let error =
                        errors::self_is_not_first_parameter(SourceOrigin::Plain(parameter.span));
                    context.errors.push(error);
                    None
                }
                ast::Parameter::Normal(parameter) => compile_normal_parameter(parameter, context),
            })
            .collect();

        let mut all_parameters = vec![self_parameter];
        all_parameters.append(&mut normal_parameters);
        context.scope.pop();

        let method = Rc::new(RefCell::new(Function::new(
            name,
            all_parameters,
            Rc::clone(&return_type),
            SourceOrigin::Plain(method_def.signature_span),
            context.program.symbol_ids_mut(),
        )));
        context
            .globals
            .register_method(&method_def, Rc::clone(&method));

        context.program.add_function(Rc::clone(&method));
        let result = current_type.borrow_mut().add_method(Rc::clone(&method));
        if let Err(error) = result {
            let error = error.into_direct_add_error(method.borrow().definition_site.unwrap());
            context.errors.push(error);
        }
    }

    fn analyze_function_def(
        &mut self,
        function_def: &mut ast::FunctionDef,
        context: &mut CompilerContext,
    ) {
        let name = function_def.name.text.clone();
        let return_type = compile_return_type(function_def.return_type.as_ref(), context);

        // Parameters have their own scope.
        context.scope.push();
        let parameters: Vec<Rc<RefCell<Variable>>> = function_def
            .parameters
            .iter()
            .flat_map(|parameter| match parameter {
                ast::Parameter::Self_(parameter) => {
                    let error = errors::self_not_in_method_signature(
                        &function_def,
                        SourceOrigin::Plain(parameter.span),
                    );
                    context.errors.push(error);
                    None
                }
                ast::Parameter::Normal(parameter) => compile_normal_parameter(parameter, context),
            })
            .collect();

        context.scope.pop();

        let function = Rc::new(RefCell::new(Function::new(
            name,
            parameters,
            Rc::clone(&return_type),
            SourceOrigin::Plain(function_def.signature_span),
            context.program.symbol_ids_mut(),
        )));
        context
            .globals
            .register_function(&function_def, Rc::clone(&function));

        context.program.add_function(Rc::clone(&function));
        let result = context.scope.add(FunctionEntity(Rc::clone(&function)));
        if let Err(error) = result {
            let error = error.into_direct_add_error(function.borrow().definition_site.unwrap());
            context.errors.push(error);
        }
    }
}

fn compile_return_type(
    return_type: Option<&ast::TypeExpr>,
    context: &mut CompilerContext,
) -> Rc<RefCell<Type>> {
    match return_type {
        Some(return_type) => type_exprs::compile_type_expr(return_type, context).into(),
        None => Rc::clone(context.program.types().void()),
    }
}

fn compile_normal_parameter(
    parameter: &ast::NormalParameter,
    context: &mut CompilerContext,
) -> Option<Rc<RefCell<Variable>>> {
    let name = parameter.name.text.clone();
    let type_ = type_exprs::compile_type_expr(&parameter.type_, context);

    create_parameter(name, type_, SourceOrigin::Plain(parameter.span), context)
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

    create_parameter(
        "<self>".to_string(),
        TypeRef::new(type_, None),
        SourceOrigin::Plain(parameter.span),
        context,
    )
    .expect("Couldn't create variable for <self> parameter")
}

fn create_parameter(
    name: String,
    type_: TypeRef,
    definition_site: SourceOrigin,
    context: &mut CompilerContext,
) -> Option<Rc<RefCell<Variable>>> {
    let variable = Rc::new(RefCell::new(Variable::new(
        name,
        type_,
        definition_site,
        &mut context.program,
    )));

    if let Err(error) = context.scope.add(VariableEntity(Rc::clone(&variable))) {
        let error = error.into_direct_add_error(definition_site);
        context.errors.push(error);
    };
    Some(variable)
}
