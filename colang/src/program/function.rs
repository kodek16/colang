use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::internal::InternalFunctionTag;
use crate::program::transforms::visitor::CodeVisitor;
use crate::program::{
    transforms, ArrayFromElementsExpr, CallExpr, Expression, FieldAccessExpr, SymbolId,
    SymbolIdRegistry, Type, TypeId, TypeRegistry, Variable,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Function {
    pub name: String,
    pub id: FunctionId,
    pub parameters: Vec<Rc<RefCell<Variable>>>,
    pub return_type: Rc<RefCell<Type>>,

    pub definition_site: Option<InputSpan>,

    // Body is wrapped in a separate cell so that we can still borrow function immutably
    // while the body is borrowed mutably. This is very useful for recursive functions.
    pub body: Option<Rc<RefCell<Expression>>>,

    /// For instantiated methods, this is the ID of their prototype method in the base type.
    pub base_method_id: Option<FunctionId>,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum FunctionId {
    UserDefined(SymbolId),
    InstantiatedMethod(SymbolId, TypeId),
    Internal(InternalFunctionTag),
}

pub struct ProtoInternalParameter {
    pub name: String,
    pub type_: Rc<RefCell<Type>>,
}

impl Function {
    /// Initialize a new, empty function. Parameters and body are filled later.
    pub fn new(
        name: String,
        return_type: Rc<RefCell<Type>>,
        definition_site: InputSpan,
        symbol_ids: &mut SymbolIdRegistry,
    ) -> Function {
        Function {
            name,
            id: FunctionId::UserDefined(symbol_ids.next_id()),
            definition_site: Some(definition_site),
            parameters: vec![],
            return_type,
            body: None,
            base_method_id: None,
        }
    }

    pub fn new_internal(
        name: String,
        tag: InternalFunctionTag,
        parameters: Vec<ProtoInternalParameter>,
        return_type: Rc<RefCell<Type>>,
    ) -> Function {
        let function_id = FunctionId::Internal(tag.clone());
        let parameters = parameters
            .into_iter()
            .enumerate()
            .map(|(index, parameter)| {
                Rc::new(RefCell::new(Variable::new_internal_parameter(
                    parameter,
                    tag.clone(),
                    index,
                )))
            })
            .collect();

        Function {
            name,
            id: function_id,
            definition_site: None,
            parameters,
            return_type,
            body: None,
            base_method_id: None,
        }
    }

    // TODO get rid of this.
    // This has to be called for user-defined functions.
    pub fn fill_parameters(&mut self, parameters: Vec<Rc<RefCell<Variable>>>) {
        self.parameters = parameters
    }

    // We need to accept `body_type` as a separate parameter, because computing
    // the type for recursive functions involves borrowing the function immutably,
    // so if we do this inside the method, where we have a mutable borrow, it would cause a panic.
    #[must_use]
    pub fn fill_body(
        &mut self,
        body: Expression,
        body_type: Rc<RefCell<Type>>,
    ) -> Result<(), CompilationError> {
        if body_type != self.return_type {
            let error = CompilationError::function_body_type_mismatch(
                &self.return_type.borrow().name(),
                &body_type.borrow().name(),
                self.definition_site
                    .expect("Attempt to fill body for internal function"),
            );
            return Err(error);
        }

        self.body = Some(Rc::new(RefCell::new(body)));
        Ok(())
    }

    pub fn body(&self) -> &Rc<RefCell<Expression>> {
        self.body.as_ref().expect("function body was not filled")
    }

    /// Create a copy of this function with all occurrences of type parameters replaced by
    /// concrete type arguments.
    pub fn instantiate(
        &self,
        instantiated_type_id: TypeId,
        type_arguments: &HashMap<TypeId, TypeId>,
        types: &mut TypeRegistry,
    ) -> Rc<RefCell<Function>> {
        match self.id {
            FunctionId::Internal(ref tag) => {
                let tag = match tag {
                    InternalFunctionTag::ArrayPush(type_id) => InternalFunctionTag::ArrayPush(
                        type_arguments.get(type_id).unwrap_or(type_id).clone(),
                    ),
                    InternalFunctionTag::ArrayPop(type_id) => InternalFunctionTag::ArrayPop(
                        type_arguments.get(type_id).unwrap_or(type_id).clone(),
                    ),
                    InternalFunctionTag::ArrayLen(type_id) => InternalFunctionTag::ArrayLen(
                        type_arguments.get(type_id).unwrap_or(type_id).clone(),
                    ),
                    InternalFunctionTag::ArrayIndex(type_id) => InternalFunctionTag::ArrayIndex(
                        type_arguments.get(type_id).unwrap_or(type_id).clone(),
                    ),
                    _ => panic!("Attempt to instantiate a non-template internal method."),
                };
                let id = FunctionId::Internal(tag.clone());

                let parameters = self
                    .parameters
                    .iter()
                    .map(|parameter| {
                        Rc::new(RefCell::new(
                            parameter.borrow().instantiate_internal_parameter(
                                tag.clone(),
                                type_arguments,
                                types,
                            ),
                        ))
                    })
                    .collect();

                let return_type = self.return_type.borrow().instantiate(type_arguments, types);

                Rc::new(RefCell::new(Function {
                    id,
                    parameters,
                    return_type,
                    name: self.name.clone(),
                    definition_site: self.definition_site,
                    body: None,
                    base_method_id: Some(self.id.clone()),
                }))
            }
            FunctionId::UserDefined(template_id) => {
                let function_id =
                    FunctionId::InstantiatedMethod(template_id, instantiated_type_id.clone());

                let mut instantiated_function = transforms::clone::clone_function(
                    self,
                    |_| FunctionId::InstantiatedMethod(template_id, instantiated_type_id),
                    &|variable, types| {
                        variable.instantiate_local_variable(
                            function_id.clone(),
                            type_arguments,
                            types,
                        )
                    },
                    types,
                );
                instantiated_function.return_type = Rc::clone(&instantiated_function.return_type)
                    .borrow()
                    .instantiate(type_arguments, types);
                instantiated_function.base_method_id = Some(self.id.clone());

                // TODO don't do it now
                {
                    let function_body = instantiated_function.body.as_ref().unwrap();
                    let mut function_body = function_body.borrow_mut();
                    let mut rewriter = InstantiatedMethodBodyRewriter {
                        types,
                        type_arguments,
                    };
                    rewriter.visit_expression(&mut function_body);
                }

                Rc::new(RefCell::new(instantiated_function))
            }
            FunctionId::InstantiatedMethod(_, _) => {
                panic!("Attempt to instantiate an already instantiated method");
            }
        }
    }

    /// For methods of template base types, looks up the method instantiation in an instantiation
    /// of the type.
    pub fn lookup_instantiated_method(&self, instantiated_type: &Type) -> Rc<RefCell<Function>> {
        let target_method_id = self.base_method_id.clone().unwrap_or(self.id.clone());

        for method in instantiated_type.methods() {
            if let Some(base_method_id) = method.borrow().base_method_id.clone() {
                if base_method_id == target_method_id {
                    return Rc::clone(method);
                }
            }
        }

        panic!(
            "Could not find an instance of method `{}` in type `{}`",
            self.name,
            instantiated_type.name()
        )
    }
}

struct InstantiatedMethodBodyRewriter<'a> {
    type_arguments: &'a HashMap<TypeId, TypeId>,
    types: &'a mut TypeRegistry,
}

impl<'a> CodeVisitor for InstantiatedMethodBodyRewriter<'a> {
    fn types(&mut self) -> &mut TypeRegistry {
        self.types
    }

    fn visit_array_from_elements_expr(&mut self, expression: &mut ArrayFromElementsExpr) {
        self.walk_array_from_elements_expr(expression);
        expression.element_type = Rc::clone(&expression.element_type)
            .borrow()
            .instantiate(self.type_arguments, self.types)
    }

    fn visit_call_expr(&mut self, expression: &mut CallExpr) {
        self.walk_call_expr(expression);
        if expression.arguments.is_empty() {
            return;
        }

        let receiver_type = expression.arguments[0].type_();
        let self_type = Rc::clone(&expression.function.borrow().parameters[0].borrow().type_);

        if *receiver_type != self_type {
            let receiver_type = match receiver_type.borrow().pointer_target_type(self.types) {
                Some(target_type) => target_type,
                None => Rc::clone(receiver_type),
            };

            let instantiated_method = expression
                .function
                .borrow()
                .lookup_instantiated_method(&receiver_type.borrow());
            expression.function = instantiated_method;
        }
    }

    fn visit_field_access_expr(&mut self, expression: &mut FieldAccessExpr) {
        self.walk_field_access_expr(expression);

        let receiver_type = expression.receiver.type_().borrow();
        if !receiver_type
            .fields()
            .any(|type_field| *type_field == expression.field)
        {
            let instantiated_field = expression
                .field
                .borrow()
                .lookup_instantiated_field(&receiver_type);
            expression.field = instantiated_field;
        }
    }
}
