use crate::ast::InputSpan;
use crate::program::function::ProtoInternalParameter;
use crate::program::{
    FunctionId, InternalFunctionTag, Program, SymbolId, Type, TypeId, TypeRegistry,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Variable {
    pub name: String,
    pub id: VariableId,
    pub definition_site: Option<InputSpan>,
    pub type_: Rc<RefCell<Type>>,

    /// For instantiated fields, this is the ID of their prototype field in the base type.
    base_field_id: Option<VariableId>,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum VariableId {
    LocalVariable(SymbolId),
    InstantiatedLocalVariable(SymbolId, FunctionId),

    Field(SymbolId),
    InstantiatedField(SymbolId, TypeId),

    InternalParameter(InternalFunctionTag, usize),
}

impl Variable {
    /// Creates a new variable with a given name and type.
    pub fn new_variable(
        name: String,
        type_: Rc<RefCell<Type>>,
        definition_site: Option<InputSpan>,
        program: &mut Program,
    ) -> Variable {
        Variable {
            name,
            type_,
            definition_site,
            id: VariableId::LocalVariable(program.symbol_ids_mut().next_id()),
            base_field_id: None,
        }
    }

    pub fn new_field(
        name: String,
        type_: Rc<RefCell<Type>>,
        definition_site: Option<InputSpan>,
        program: &mut Program,
    ) -> Variable {
        Variable {
            name,
            type_,
            definition_site,
            id: VariableId::Field(program.symbol_ids_mut().next_id()),
            base_field_id: None,
        }
    }

    pub fn new_internal_parameter(
        parameter: ProtoInternalParameter,
        function_tag: InternalFunctionTag,
        parameter_index: usize,
    ) -> Variable {
        Variable {
            name: parameter.name,
            id: VariableId::InternalParameter(function_tag, parameter_index),
            type_: parameter.type_,
            definition_site: None,
            base_field_id: None,
        }
    }

    /// Create a copy of this field with all occurrences of type parameters in its type
    /// replaced by concrete type arguments.
    pub fn instantiate_field(
        &self,
        instantiated_type_id: TypeId,
        type_arguments: &HashMap<TypeId, TypeId>,
        types: &mut TypeRegistry,
    ) -> Variable {
        let id = match self.id {
            VariableId::Field(id) => VariableId::InstantiatedField(id, instantiated_type_id),
            _ => panic!("Attempt to instantiate variable that is not a field"),
        };

        Variable {
            id,
            name: self.name.clone(),
            definition_site: self.definition_site,
            type_: Type::substitute(&self.type_, type_arguments, types),
            base_field_id: Some(self.id.clone()),
        }
    }

    pub fn instantiate_local_variable(
        &self,
        instantiated_function_id: FunctionId,
        type_arguments: &HashMap<TypeId, TypeId>,
        types: &mut TypeRegistry,
    ) -> Variable {
        let id = match self.id {
            VariableId::LocalVariable(id) => {
                VariableId::InstantiatedLocalVariable(id, instantiated_function_id.clone())
            }
            _ => panic!("Attempt to instantiate a non-local variable"),
        };

        Variable {
            id,
            name: self.name.clone(),
            definition_site: self.definition_site,
            type_: Type::substitute(&self.type_, type_arguments, types),
            base_field_id: None,
        }
    }

    pub fn instantiate_internal_parameter(
        &self,
        instantiated_function_tag: InternalFunctionTag,
        type_arguments: &HashMap<TypeId, TypeId>,
        types: &mut TypeRegistry,
    ) -> Variable {
        let id = match self.id {
            VariableId::InternalParameter(_, index) => {
                VariableId::InternalParameter(instantiated_function_tag, index)
            }
            _ => panic!("Attempt to instantiate variable that is not an internal parameter"),
        };

        Variable {
            id,
            name: self.name.clone(),
            definition_site: self.definition_site,
            type_: Type::substitute(&self.type_, type_arguments, types),
            base_field_id: None,
        }
    }

    /// For fields of template base types, looks up the field instantiation in an instantiation
    /// of the type.
    pub fn lookup_instantiated_field(&self, instantiated_type: &Type) -> Rc<RefCell<Variable>> {
        let target_field_id = self.base_field_id.clone().unwrap_or(self.id.clone());

        for field in instantiated_type.fields() {
            if let Some(base_field_id) = field.borrow().base_field_id.clone() {
                if base_field_id == target_field_id {
                    return Rc::clone(field);
                }
            }
        }

        panic!(
            "Could not find an instance of field `{}` in type `{}`",
            self.name, instantiated_type.name
        )
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
