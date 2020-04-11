use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::internal::InternalFunctionTag;
use crate::program::{
    Expression, SymbolId, SymbolIdRegistry, Type, TypeId, TypeRegistry, Variable,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Function {
    pub name: String,
    pub id: FunctionId,
    pub definition_site: Option<InputSpan>,
    pub parameters: Vec<Rc<RefCell<Variable>>>,
    pub return_type: Rc<RefCell<Type>>,

    pub body: Option<Expression>,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum FunctionId {
    UserDefined(SymbolId),
    Internal(InternalFunctionTag),
}

impl Function {
    /// Initialize a new, empty function. Parameters and body are filled later.
    pub fn new_user_defined(
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
        }
    }

    pub fn is_user_defined(&self) -> bool {
        match self.id {
            FunctionId::UserDefined(_) => true,
            FunctionId::Internal(_) => false,
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

        self.body = Some(body);
        Ok(())
    }

    pub fn body(&self) -> &Expression {
        &self.body.as_ref().expect("function body was not filled")
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
                    other => other.clone(),
                };
                let id = FunctionId::Internal(tag.clone());

                let parameters = self
                    .parameters
                    .iter()
                    .map(|parameter| {
                        parameter.borrow().instantiate_internal_parameter(
                            tag.clone(),
                            type_arguments,
                            types,
                        )
                    })
                    .collect();

                let return_type = self.return_type.borrow().instantiate(type_arguments, types);

                Rc::new(RefCell::new(Function {
                    name: self.name.clone(),
                    id,
                    definition_site: self.definition_site,
                    parameters,
                    return_type,
                    body: None,
                }))
            }
            FunctionId::UserDefined(_) => unimplemented!(),
        }
    }
}

pub struct ProtoInternalParameter {
    pub name: String,
    pub type_: Rc<RefCell<Type>>,
}
