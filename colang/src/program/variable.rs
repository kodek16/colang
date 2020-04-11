use crate::ast::InputSpan;
use crate::errors::CompilationError;
use crate::program::function::ProtoInternalParameter;
use crate::program::{InternalFunctionTag, Program, SymbolId, Type, TypeId, TypeRegistry};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::Deref;
use std::rc::Rc;

pub struct Variable {
    pub name: String,
    pub definition_site: Option<InputSpan>,
    pub id: VariableId,
    pub(crate) type_: Rc<RefCell<Type>>,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum VariableId {
    LocalVariable(SymbolId),
    InstantiatedLocalVariable(SymbolId, TypeId),

    Field(SymbolId),
    InstantiatedField(SymbolId, TypeId),

    InternalParameter(InternalFunctionTag, usize),
}

impl Variable {
    /// Creates a new variable with a given name and type.
    pub fn new(
        name: String,
        type_: Rc<RefCell<Type>>,
        definition_site: Option<InputSpan>,
        program: &mut Program,
    ) -> Result<Variable, CompilationError> {
        if type_ == *program.types().void() {
            let error = CompilationError::variable_of_type_void(
                definition_site.expect("Internal variable of type `void` defined."),
            );
            return Err(error);
        }

        Ok(Variable {
            name,
            type_,
            definition_site,
            id: VariableId::LocalVariable(program.symbol_ids_mut().next_id()),
        })
    }

    pub fn new_field(
        name: String,
        type_: Rc<RefCell<Type>>,
        definition_site: Option<InputSpan>,
        program: &mut Program,
    ) -> Result<Variable, CompilationError> {
        if type_ == *program.types().void() {
            let error = CompilationError::variable_of_type_void(
                definition_site.expect("Internal field of type `void` defined."),
            );
            return Err(error);
        }

        Ok(Variable {
            name,
            type_,
            definition_site,
            id: VariableId::Field(program.symbol_ids_mut().next_id()),
        })
    }

    pub fn new_internal_parameter(
        parameter: ProtoInternalParameter,
        function_tag: InternalFunctionTag,
        parameter_index: usize,
    ) -> Variable {
        Variable {
            name: parameter.name,
            type_: parameter.type_,
            definition_site: None,
            id: VariableId::InternalParameter(function_tag, parameter_index),
        }
    }

    pub fn type_(&self) -> impl Deref<Target = Type> + '_ {
        self.type_.borrow()
    }

    /// Create a copy of this field with all occurrences of type parameters in its type
    /// replaced by concrete type arguments.
    pub fn instantiate_field(
        &self,
        instantiated_type_id: TypeId,
        type_arguments: &HashMap<TypeId, TypeId>,
        types: &mut TypeRegistry,
    ) -> Rc<RefCell<Variable>> {
        let id = match self.id {
            VariableId::Field(id) => VariableId::InstantiatedField(id, instantiated_type_id),
            _ => panic!("Attempt to instantiate variable that is not a field"),
        };

        Rc::new(RefCell::new(Variable {
            id,
            name: self.name.clone(),
            definition_site: self.definition_site,
            type_: self.type_.borrow().instantiate(type_arguments, types),
        }))
    }

    pub fn instantiate_internal_parameter(
        &self,
        instantiated_function_tag: InternalFunctionTag,
        type_arguments: &HashMap<TypeId, TypeId>,
        types: &mut TypeRegistry,
    ) -> Rc<RefCell<Variable>> {
        let id = match self.id {
            VariableId::InternalParameter(_, index) => {
                VariableId::InternalParameter(instantiated_function_tag, index)
            }
            _ => panic!("Attempt to instantiate variable that is not an internal parameter"),
        };

        Rc::new(RefCell::new(Variable {
            id,
            name: self.name.clone(),
            definition_site: self.definition_site,
            type_: self.type_.borrow().instantiate(type_arguments, types),
        }))
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
