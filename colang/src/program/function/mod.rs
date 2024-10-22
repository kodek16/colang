//! Definitions and handling routines for CO functions (including methods).

mod clone;
mod signature;

use crate::program::dual::call::Call;
use crate::program::expressions::array_from_elements::ArrayFromElementsExpr;
use crate::program::expressions::field_access::FieldAccessExpr;
use crate::program::expressions::new::NewExpr;
use crate::program::expressions::null::NullExpr;
use crate::program::internal::InternalFunctionTag;
use crate::program::symbols::SymbolIdRegistry;
use crate::program::visitors::LocalVisitor;
use crate::program::{Block, Statement};
use crate::program::{SymbolId, Type, TypeId, TypeRegistry, Variable};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub use signature::Signature;

/// A function in CO: an executable subroutine in the program.
///
/// CO functions are executable subroutines that can be _called_ by providing an argument
/// for every parameter in the function definition.
///
/// Functions can return a value from their body which gets passed to the caller. The type of the
/// returned value has to specified in the function signature as `return_type`. Functions with a
/// return type are called _non-void functions_, and functions without a return type (i.e. those
/// that do not return a value) are called _void functions_.
///
/// A call to a void function is a statement. A call to a non-void function is an expression that
/// ultimately evaluates to the return value of the function.
///
/// Functions can be divided into two syntactic categories: _free functions_ which are defined in
/// the global scope, and _methods_ which are defined in the context of some type. From a semantic
/// perspective, there is no difference between a free function and a method, so the term "function"
/// usually means any of them. If there is a need to specify one of the categories, the terms
/// "method" and "non-method function" can be used.
///
/// Methods must be defined with a special first parameter named `self`, and called with a special
/// syntax that includes a "receiver" object: `x.f(y)` is a call to a method `f` of type of `x`
/// that has two parameters. The `self` parameter is set to `x` and the other parameter set to `y`.
///
/// The `self` parameter can have a special form `&self` that indicates that the receiver must be
/// passed as a pointer. Methods that use this form can only be called on lvalue receivers (which
/// are automatically referenced).
pub struct Function {
    /// The name of the function.
    pub name: String,

    /// A unique identifier of the function.
    pub id: FunctionId,

    /// Function parameters.
    ///
    /// Every parameter is associated with a `Variable` object representing a local variable that
    /// gets assigned the value of the argument at call site.
    pub parameters: Vec<Rc<RefCell<Variable>>>,

    /// The return type of the function.
    ///
    /// For functions that do not return a value ("void functions"), this is `None`.
    pub return_type: Option<Rc<RefCell<Type>>>,

    /// The location where function was defined.
    ///
    /// This can be `None` for internal functions.
    pub definition_site: Option<SourceOrigin>,

    /// Current state of the function body.
    pub(crate) body: FunctionBody,

    /// For instantiated methods, this is their prototype method in the base type.
    base_method: Option<Rc<RefCell<Function>>>,

    /// For methods, these are the trait methods that they implement.
    ///
    /// A method may implement methods from different traits if they require the same signature.
    implemented_methods: Vec<Rc<RefCell<Function>>>,
}

/// A plain, hashable, unique identifier for functions.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum FunctionId {
    UserDefined(SymbolId),
    InstantiatedMethod(SymbolId, TypeId),
    Internal(InternalFunctionTag),
}

/// A stateful wrapper for function bodies that can be in one of a few states.
pub(crate) enum FunctionBody {
    /// Function body is fully analyzed and ready to be used.
    ///
    /// If a function is non-void, its body must be terminated with a `return` statement.
    // Body is wrapped in a separate cell so that we can still borrow function immutably
    // while the body is borrowed mutably. This is very useful for recursive functions.
    Filled(Rc<RefCell<Statement>>),

    /// Function body is expected to be filled in a future analysis pass.
    ToBeFilled,

    /// Function body is expected to be instantiated from the base function body.
    ///
    /// Instantiation is handled by `colang::analyzer::function_instantiations`.
    ToBeInstantiated {
        base: Rc<RefCell<Function>>,
        type_arguments: HashMap<TypeId, TypeId>,
    },

    /// Function body is deliberately absent because the function is a trait method.
    TraitMethod,

    /// Function body is deliberately absent because the function is internal.
    Internal,
}

/// Information about an internal parameter of some function.
pub struct ProtoInternalParameter {
    pub name: String,
    pub type_: Rc<RefCell<Type>>,
}

impl Function {
    /// Creates a new user-defined function without a body.
    ///
    /// The function body is expected to be filled later.
    pub fn new(
        name: String,
        parameters: Vec<Rc<RefCell<Variable>>>,
        return_type: Option<Rc<RefCell<Type>>>,
        definition_site: SourceOrigin,
        symbol_ids: &mut SymbolIdRegistry,
    ) -> Function {
        Function {
            name,
            parameters,
            return_type,
            id: FunctionId::UserDefined(symbol_ids.next_id()),
            definition_site: Some(definition_site),
            body: FunctionBody::ToBeFilled,
            base_method: None,
            implemented_methods: Vec::new(),
        }
    }

    /// Creates a new internal function.
    pub fn new_internal(
        name: String,
        tag: InternalFunctionTag,
        parameters: Vec<ProtoInternalParameter>,
        return_type: Option<Rc<RefCell<Type>>>,
    ) -> Function {
        let id = FunctionId::Internal(tag.clone());
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
            id,
            parameters,
            return_type,
            definition_site: None,
            body: FunctionBody::Internal,
            base_method: None,
            implemented_methods: Vec::new(),
        }
    }

    pub fn signature(&self) -> Signature {
        Signature {
            parameters: self
                .parameters
                .iter()
                .map(|parameter| Rc::clone(&parameter.borrow().type_))
                .collect(),
            return_type: self.return_type.clone(),
        }
    }

    /// Checks if the function is a "void function".
    ///
    /// Void functions are functions that do not return a value.
    pub fn is_void(&self) -> bool {
        self.return_type.is_none()
    }

    /// Accesses the function body which is assumed to be present.
    ///
    /// This method should only be called for user-defined functions only after
    /// the analysis is complete.
    pub fn body(&self) -> &Rc<RefCell<Statement>> {
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

    /// Checks if the function body is expecting instantiation.
    pub fn body_needs_instantiation(&self) -> bool {
        match &self.body {
            FunctionBody::ToBeInstantiated { .. } => true,
            _ => false,
        }
    }

    /// Wires a method of a type with a method of a trait that is being implemented.
    pub fn wire_with_trait_method(&mut self, trait_method: Rc<RefCell<Function>>) {
        self.implemented_methods.push(trait_method)
    }

    /// Instantiates the function, creating a copy after applying some type substitutions.
    ///
    /// Does not actually instantiate the function body yet, this needs to be done in a later
    /// pass by calling `instantiate_body`.
    ///
    /// A copy of the function is created with all occurrences of type parameters in its signature
    /// replaced by concrete type arguments.
    ///
    /// Currently only methods can be instantiated, and `instantiated_type_id` is used to create
    /// unique IDs for them. In the future this is likely to change as function templates are
    /// introduced.
    pub fn instantiate_interface(
        function: Rc<RefCell<Function>>,
        instantiated_type_id: TypeId,
        type_arguments: &HashMap<TypeId, TypeId>,
        types: &mut TypeRegistry,
    ) -> Rc<RefCell<Function>> {
        let base_method = Rc::clone(&function);

        let return_type = function
            .borrow()
            .return_type
            .as_ref()
            .map(|return_type| Type::substitute(return_type, type_arguments, types));

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

                Rc::new(RefCell::new(Function {
                    id,
                    parameters,
                    return_type,
                    name: function.name.clone(),
                    definition_site: function.definition_site,
                    body: FunctionBody::Internal,
                    base_method: Some(base_method),

                    // TODO(#25): this may require non-trivial rewiring if traits depend on type
                    // parameters of the receiver type.
                    implemented_methods: function.implemented_methods.clone(),
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

                Rc::new(RefCell::new(Function {
                    name: function.name.clone(),
                    id: function_id,
                    definition_site: function.definition_site,
                    parameters,
                    return_type,
                    base_method: Some(base_method),
                    body,

                    // TODO(#25): this may require non-trivial rewiring if traits depend on type
                    // parameters of the receiver type.
                    implemented_methods: function.implemented_methods.clone(),
                }))
            }
            FunctionId::InstantiatedMethod(_, _) => {
                panic!("Attempt to instantiate an already instantiated method");
            }
        }
    }

    /// Instantiates the function body, copying it from the body of the base function.
    ///
    /// It is assumed that `function` was instantiated using `instantiate_interface` and so has
    /// the necessary information to instantiate its body.
    ///
    /// The new body is obtained by applying type substitutions to the entire code of the base
    /// function body, rerouting field accesses and method calls to other types where necessary.
    /// These transformations are guaranteed to be safe as long as type arguments conform to
    /// their corresponding type parameters.
    pub fn instantiate_body(
        function: Rc<RefCell<Function>>,
        types: &mut TypeRegistry,
    ) -> Rc<RefCell<Statement>> {
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

                let mut new_body = clone::clone_function_body(
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
                rewriter.visit_statement(&mut new_body);

                let new_body = Rc::new(RefCell::new(new_body));
                function.borrow_mut().body = FunctionBody::Filled(Rc::clone(&new_body));
                new_body
            }
            _ => panic!("Attempt to instantiate body of a function that was not instantiated"),
        }
    }

    /// Looks up the method of the same origin as this method in another type.
    ///
    /// Assumes that this function is a method.
    ///
    /// "Rewiring" in general refers to redirecting call expressions inside function templates
    /// to functions that work with the argument types after type substitution. This method
    /// uses the following connections between methods to determine the rewiring result:
    ///
    /// 1) If `self` is a method of some type template instance `Foo<A...>`, and `receiver_type`
    ///    is a different instance of the same template (`Foo<B...>`), then the rewiring result
    ///    is the instantiation of the same base method in base type of `Foo` as `self`.
    ///
    /// 2) If `self` implements a trait method of some trait `Bar`, and `receiver_type` implements
    ///    `Bar`, then the result is the implementation of the same trait method in the
    ///    `receiver_type`.
    pub fn rewire_method(&self, receiver_type: &Type) -> Rc<RefCell<Function>> {
        fn rewire_through_base_type(
            method: &Function,
            receiver_type: &Type,
        ) -> Option<Rc<RefCell<Function>>> {
            let target_method_id = method
                .base_method
                .as_ref()
                .map(|method| method.borrow().id.clone())
                .unwrap_or(method.id.clone());

            for method in &receiver_type.methods {
                if let Some(base_method) = method.borrow().base_method.clone() {
                    if base_method.borrow().id == target_method_id {
                        return Some(Rc::clone(method));
                    }
                }
            }

            None
        }

        fn rewire_through_trait(
            method: &Function,
            receiver_type: &Type,
        ) -> Option<Rc<RefCell<Function>>> {
            for trait_method in &method.implemented_methods {
                for method in &receiver_type.methods {
                    if method.borrow().implemented_methods.contains(trait_method) {
                        return Some(Rc::clone(&method));
                    }
                }
            }

            None
        }

        rewire_through_base_type(self, receiver_type)
            .or_else(|| rewire_through_trait(self, receiver_type))
            .unwrap_or_else(|| {
                panic!(
                    "Could not rewire method `{}` to receiver type `{}`",
                    self.name, receiver_type.name
                )
            })
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

impl<'a> LocalVisitor for InstantiatedMethodBodyRewriter<'a> {
    fn types(&mut self) -> &mut TypeRegistry {
        self.types
    }

    fn visit_array_from_elements_expr(&mut self, expression: &mut ArrayFromElementsExpr) {
        self.walk(expression);
        expression.element_type =
            Type::substitute(&expression.element_type, self.type_arguments, self.types);
    }

    fn visit_field_access_expr(&mut self, expression: &mut FieldAccessExpr) {
        self.walk(expression);

        let receiver_type = expression.receiver.type_().borrow();
        if !receiver_type
            .fields
            .iter()
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
        self.walk(expression);

        expression.target_type =
            Type::substitute(&expression.target_type, self.type_arguments, self.types);
    }

    fn visit_null_expr(&mut self, expression: &mut NullExpr) {
        self.walk(expression);

        expression.target_type =
            Type::substitute(&expression.target_type, self.type_arguments, self.types);
    }

    fn visit_block(&mut self, block: &mut Block) {
        for variable in &block.local_variables {
            let variable = variable.borrow();

            // The base variable type and the type arguments are assumed to be fully complete at this
            // point, so we can assume no errors will occur.
            // We do it here and not in `Variable::instantiate_local_variable` because running this code
            // for method parameters is likely to cause a stack overflow by infinite loop.
            Type::ensure_is_fully_complete(Rc::clone(&variable.type_), self.types)
                .map_err(|_| ())
                .expect(&format!(
                    "Infinite type chain encountered while instantiating variable `{}` of type `{}",
                    variable.name,
                    variable.type_.borrow().name
                ));
        }

        self.walk(block);
    }

    fn visit_call(&mut self, expression: &mut Call) {
        self.walk(expression);
        if expression.arguments.is_empty() {
            return;
        }

        let receiver_type = expression.arguments[0].type_();
        let self_type = Rc::clone(&expression.function.borrow().parameters[0].borrow().type_);

        if *receiver_type != self_type {
            let receiver_type = match receiver_type.borrow().pointer_target_type() {
                Some(target_type) => target_type,
                None => Rc::clone(receiver_type),
            };

            let rewired_method = expression
                .function
                .borrow()
                .rewire_method(&receiver_type.borrow());
            expression.function = rewired_method;
        }
    }
}
