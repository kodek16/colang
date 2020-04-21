use crate::program::internal::InternalFunctionTag;
use crate::program::transforms::visitor::CodeVisitor;
use crate::program::{
    transforms, ArrayFromElementsExpr, CallExpr, Expression, FieldAccessExpr, NewExpr, NullExpr,
    SymbolId, SymbolIdRegistry, Type, TypeId, TypeRegistry, Variable,
};
use crate::source::{InputSpan, SourceOrigin};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Function {
    pub name: String,
    pub id: FunctionId,
    pub parameters: Vec<Rc<RefCell<Variable>>>,
    pub return_type: Rc<RefCell<Type>>,

    pub definition_site: Option<SourceOrigin>,

    pub body: FunctionBody,

    /// For instantiated methods, this is the ID of their prototype method in the base type.
    pub base_method_id: Option<FunctionId>,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum FunctionId {
    UserDefined(SymbolId),
    InstantiatedMethod(SymbolId, TypeId),
    Internal(InternalFunctionTag),
}

pub enum FunctionBody {
    // Body is wrapped in a separate cell so that we can still borrow function immutably
    // while the body is borrowed mutably. This is very useful for recursive functions.
    Filled(Rc<RefCell<Expression>>),
    ToBeFilled,
    ToBeInstantiated {
        base: Rc<RefCell<Function>>,
        type_arguments: HashMap<TypeId, TypeId>,
    },
    Internal,
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
            definition_site: Some(SourceOrigin::Plain(definition_site)),
            parameters: vec![],
            return_type,
            body: FunctionBody::ToBeFilled,
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
            body: FunctionBody::Internal,
            base_method_id: None,
        }
    }

    // TODO get rid of this.
    // This has to be called for user-defined functions.
    pub fn fill_parameters(&mut self, parameters: Vec<Rc<RefCell<Variable>>>) {
        self.parameters = parameters
    }

    pub fn body(&self) -> &Rc<RefCell<Expression>> {
        match &self.body {
            FunctionBody::Filled(ref body) => body,
            _ => {
                panic!(
                    "Body of user-defined function `{}` ({:?}) has not been filled",
                    self.name, self.id
                );
            }
        }
    }

    pub fn body_needs_instantiation(&self) -> bool {
        match &self.body {
            FunctionBody::ToBeInstantiated { .. } => true,
            _ => false,
        }
    }

    /// Create a copy of a function with all occurrences of type parameters replaced by
    /// concrete type arguments.
    /// The function body is not copied yet, it has to be done later by calling `instantiate_body`.
    pub fn instantiate_interface(
        function: Rc<RefCell<Function>>,
        instantiated_type_id: TypeId,
        type_arguments: &HashMap<TypeId, TypeId>,
        types: &mut TypeRegistry,
    ) -> Rc<RefCell<Function>> {
        match function.borrow().id {
            FunctionId::Internal(ref tag) => {
                let function = function.borrow();
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

                let parameters = function
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

                let return_type = Type::substitute(&function.return_type, type_arguments, types);

                Rc::new(RefCell::new(Function {
                    id,
                    parameters,
                    return_type,
                    name: function.name.clone(),
                    definition_site: function.definition_site,
                    body: FunctionBody::Internal,
                    base_method_id: Some(function.id.clone()),
                }))
            }
            FunctionId::UserDefined(template_id) => {
                let body = FunctionBody::ToBeInstantiated {
                    base: Rc::clone(&function),
                    type_arguments: type_arguments.clone(),
                };
                let function = function.borrow();

                let function_id =
                    FunctionId::InstantiatedMethod(template_id, instantiated_type_id.clone());
                let parameters: Vec<_> = function
                    .parameters
                    .iter()
                    .map(|parameter| {
                        Rc::new(RefCell::new(parameter.borrow().instantiate_local_variable(
                            function_id.clone(),
                            type_arguments,
                            types,
                        )))
                    })
                    .collect();
                let return_type = Type::substitute(&function.return_type, type_arguments, types);
                let base_method_id = Some(function.id.clone());

                Rc::new(RefCell::new(Function {
                    name: function.name.clone(),
                    id: function_id,
                    definition_site: function.definition_site,
                    parameters,
                    return_type,
                    base_method_id,
                    body,
                }))
            }
            FunctionId::InstantiatedMethod(_, _) => {
                panic!("Attempt to instantiate an already instantiated method");
            }
        }
    }

    pub fn instantiate_body(
        function: Rc<RefCell<Function>>,
        types: &mut TypeRegistry,
    ) -> Rc<RefCell<Expression>> {
        // Extract body from function.
        let mut body = FunctionBody::ToBeFilled;
        std::mem::swap(&mut body, &mut function.borrow_mut().body);

        let function_id = function.borrow().id.clone();

        match body {
            FunctionBody::ToBeInstantiated {
                base,
                type_arguments,
            } => {
                let base = base.borrow();
                let base_body = match base.body {
                    FunctionBody::Filled(ref body) => Rc::clone(&body),
                    _ => panic!("Base function body has not been filled"),
                };

                let mut new_body = transforms::clone::clone_function_body(
                    base_body,
                    &base.parameters,
                    &function.borrow().parameters,
                    &|variable, types| {
                        variable.instantiate_local_variable(
                            function_id.clone(),
                            &type_arguments,
                            types,
                        )
                    },
                    types,
                );

                let mut rewriter = InstantiatedMethodBodyRewriter {
                    types,
                    type_arguments: &type_arguments,
                };
                rewriter.visit_expression(&mut new_body);

                let new_body = Rc::new(RefCell::new(new_body));
                function.borrow_mut().body = FunctionBody::Filled(Rc::clone(&new_body));
                new_body
            }
            _ => panic!("Attempt to instantiate body of a function that was not instantiated"),
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
            self.name, instantiated_type.name
        )
    }
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
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
        expression.element_type =
            Type::substitute(&expression.element_type, self.type_arguments, self.types);
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

    fn visit_new_expr(&mut self, expression: &mut NewExpr) {
        self.walk_new_expr(expression);

        expression.target_type =
            Type::substitute(&expression.target_type, self.type_arguments, self.types);
    }

    fn visit_null_expr(&mut self, expression: &mut NullExpr) {
        self.walk_null_expr(expression);

        expression.target_type =
            Type::substitute(&expression.target_type, self.type_arguments, self.types);
    }

    fn visit_local_variable(&mut self, variable: &mut Variable) {
        // The base variable type and the type arguments are assumed to be fully complete at this
        // point, so we can assume no errors will occur.
        // We do it here and not in `instantiate_local_variable` because running this code
        // for method parameters is likely to cause a stack overflow by infinite loop.
        Type::ensure_is_fully_complete(Rc::clone(&variable.type_), self.types)
            .map_err(|_| ())
            .expect(&format!(
                "Infinite type chain encountered while instantiating variable `{}` of type `{}",
                variable.name,
                variable.type_.borrow().name
            ));
    }
}
