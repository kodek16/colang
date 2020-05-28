//! The trait wiring analyzer pass connects trait methods to their implementations.
//!
//! This pass also fills type parameter placeholder types (TPPTs) with method definition stubs
//! from their trait bounds.

use crate::analyzer::GlobalVisitor;
use crate::ast::TypeDef;
use crate::context::CompilerContext;
use crate::program::{
    Function, Program, Trait, TraitRef, Type, TypeId, TypeRef, TypeTemplate, Variable,
};
use crate::{ast, errors};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct TraitWiringAnalyzerPass;

impl GlobalVisitor for TraitWiringAnalyzerPass {
    fn revisit_non_template_struct_def(
        &mut self,
        _: &mut TypeDef,
        type_: Rc<RefCell<Type>>,
        context: &mut CompilerContext,
    ) {
        // Clone to allow mutable borrow of `type_` during processing.
        let traits = type_.borrow().implemented_traits.clone();

        for trait_ in traits {
            for trait_method in &trait_.borrow().self_type.borrow().methods {
                wire_trait_method(&type_, &trait_, &trait_method, context);
            }
        }
    }

    fn revisit_template_struct_def(
        &mut self,
        struct_def: &mut TypeDef,
        template: Rc<RefCell<TypeTemplate>>,
        context: &mut CompilerContext,
    ) {
        let base_type = Rc::clone(template.borrow().base_type());

        // Clone to allow mutable borrow of `base_type` during processing.
        let traits = base_type.borrow().implemented_traits.clone();

        for trait_ in traits {
            for trait_method in &trait_.borrow().self_type.borrow().methods {
                wire_trait_method(&base_type, &trait_, &trait_method, context);
            }
        }

        for (type_parameter, type_parameter_def) in (&template.borrow().type_parameters)
            .iter()
            .zip(struct_def.type_parameters.iter())
        {
            // Clone to allow mutable borrow of `type_parameter` during processing.
            let trait_bounds = type_parameter.borrow().implemented_traits.clone();

            for trait_bound in trait_bounds {
                extend_type_parameter_placeholder_with_trait_methods(
                    type_parameter_def,
                    type_parameter,
                    &trait_bound,
                    context,
                )
            }
        }
    }
}

fn wire_trait_method(
    type_: &Rc<RefCell<Type>>,
    trait_: &TraitRef,
    trait_method: &Rc<RefCell<Function>>,
    context: &mut CompilerContext,
) {
    let substitutions = trait_self_type_substitution_map(&trait_.borrow(), &type_.borrow());
    let implementation = {
        let trait_method = trait_method.borrow();

        let implementation = type_.borrow().lookup_method(&trait_method.name);
        match implementation {
            Ok(implementation_) => {
                let implementation = implementation_.borrow();
                let actual_signature = implementation.signature();
                let expected_signature = trait_method
                    .signature()
                    .substitute(&substitutions, context.program.types_mut());

                if actual_signature == expected_signature {
                    Some(Rc::clone(&implementation_))
                } else {
                    let error = errors::trait_method_signature_mismatch(
                        &trait_.borrow(),
                        &implementation,
                        &actual_signature,
                        &expected_signature,
                    );
                    context.errors.push(error);
                    None
                }
            }
            Err(_) => {
                let error =
                    errors::trait_method_not_implemented(&type_.borrow(), trait_, &trait_method);
                context.errors.push(error);
                None
            }
        }
    };

    if let Some(implementation) = implementation {
        implementation
            .borrow_mut()
            .wire_with_trait_method(Rc::clone(trait_method));
    } else {
        let implementation = generate_synthetic_trait_method_implementation(
            &type_,
            trait_,
            trait_method,
            &mut context.program,
        );
        type_.borrow_mut().methods.push(implementation);
    }
}

fn extend_type_parameter_placeholder_with_trait_methods(
    type_parameter_def: &ast::TypeParameter,
    type_parameter: &Rc<RefCell<Type>>,
    trait_: &Rc<RefCell<Trait>>,
    context: &mut CompilerContext,
) {
    for trait_method in &trait_.borrow().self_type.borrow().methods {
        let existing_implementation = type_parameter
            .borrow()
            .lookup_method(&trait_method.borrow().name)
            .ok();

        let new_implementation = generate_synthetic_trait_method_implementation(
            type_parameter,
            trait_,
            trait_method,
            &mut context.program,
        );

        // If there is already a stub with the same signature, assume it implements the new trait
        // method as well.
        if let Some(existing_implementation) = existing_implementation {
            if existing_implementation.borrow().signature()
                == new_implementation.borrow().signature()
            {
                existing_implementation
                    .borrow_mut()
                    .wire_with_trait_method(Rc::clone(trait_method));
            } else {
                let error = errors::conflicting_method_from_trait_bounds(
                    type_parameter_def,
                    &new_implementation.borrow(),
                    &existing_implementation.borrow(),
                );
                context.errors.push(error);
            }
        } else {
            type_parameter
                .borrow_mut()
                .add_method(new_implementation)
                .unwrap();
        }
    }
}

/// Generates a synthetic trait method implementation.
fn generate_synthetic_trait_method_implementation(
    type_: &Rc<RefCell<Type>>,
    trait_: &Rc<RefCell<Trait>>,
    trait_method: &Rc<RefCell<Function>>,
    program: &mut Program,
) -> Rc<RefCell<Function>> {
    let substitutions = trait_self_type_substitution_map(&trait_.borrow(), &type_.borrow());
    let location = type_.borrow().definition_site.unwrap();

    let parameters = trait_method
        .borrow()
        .parameters
        .iter()
        .map(|trait_parameter| {
            let trait_parameter = trait_parameter.borrow();
            let type_ =
                Type::substitute(&trait_parameter.type_, &substitutions, program.types_mut());
            Rc::new(RefCell::new(Variable::new(
                trait_parameter.name.clone(),
                TypeRef::new(type_, trait_parameter.type_.reference_location()),
                location,
                program,
            )))
        })
        .collect();

    let return_type = Type::substitute(
        &trait_method.borrow().return_type,
        &substitutions,
        program.types_mut(),
    );

    let implementation = Rc::new(RefCell::new(Function::new(
        trait_method.borrow().name.clone(),
        parameters,
        return_type,
        location,
        program.symbol_ids_mut(),
    )));

    implementation
        .borrow_mut()
        .wire_with_trait_method(Rc::clone(trait_method));

    implementation
}

/// Creates a type substitution `HashMap` for replacing `Self`-type with the implementor type.
fn trait_self_type_substitution_map(trait_: &Trait, type_: &Type) -> HashMap<TypeId, TypeId> {
    [(
        trait_.self_type.borrow().type_id.clone(),
        type_.type_id.clone(),
    )]
    .iter()
    .cloned()
    .collect()
}
