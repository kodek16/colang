//! CO variables, fields, and other kinds of named values.

use crate::program::function::ProtoInternalParameter;
use crate::program::{
    FunctionId, InternalFunctionTag, Program, SymbolId, Type, TypeId, TypeRegistry,
};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A variable-like entity in CO: a named, stateful value.
///
/// `Variable` may represent one of several distinct but similar kinds of entities:
///
/// - **Local variable** (or simply **variable**): a named "box" for a value of some determined type
///   defined within a function body (or more specifically, directly in a block expression).
///   Local variables instances (not in template sense) get created and destroyed automatically when
///   their enclosing block starts and finishes its execution.
/// - **Function parameter** (or simply **parameter**): a local variable that is part of a function
///   signature which gets assigned the value of the corresponding argument provided at the
///   function call site.
/// - **Field**: a part of a value of some `struct` type that can be individually referenced and
///   assigned to. Its lifecycle is fully bound to the `struct` value it is part of.
///
/// All of these entity kinds share the same common properties: they have a name, a statically
/// determined type, and they identify a stateful value.
pub struct Variable {
    /// The name of the entity.
    pub name: String,

    /// A unique identifier of the entity that also includes information about its kind.
    pub id: VariableId,

    /// The location in program source code where the entity is defined.
    ///
    /// This can be `None` only for internal function parameters.
    pub definition_site: Option<SourceOrigin>,

    /// The type of the entity value.
    pub type_: Rc<RefCell<Type>>,

    /// For instantiated fields, this is the ID of their prototype field in the base type.
    base_field_id: Option<VariableId>,
}

/// A plain, hashable, unique identifier for variable-like entities.
///
/// Also includes information about what kind of variable-like entity is a `Variable` exactly:
/// different variants correspond to different kinds.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum VariableId {
    /// Identifies a local variable defined in a function which has not been instantiated.
    LocalVariable(SymbolId),

    /// Identifies a local variable defined in an instantiated function.
    InstantiatedLocalVariable(SymbolId, FunctionId),

    /// Identifies a field of a type that has not been instantiated.
    Field(SymbolId),

    /// Identifies a field of an instantiated type.
    InstantiatedField(SymbolId, TypeId),

    /// Identifies a parameter of an internal function.
    ///
    /// Second element is the index of the parameter, starting from zero.
    InternalParameter(InternalFunctionTag, usize),
}

impl Variable {
    /// Creates a new variable with a given name and type.
    pub fn new_variable(
        name: String,
        type_: Rc<RefCell<Type>>,
        definition_site: SourceOrigin,
        program: &mut Program,
    ) -> Variable {
        Variable {
            name,
            type_,
            definition_site: Some(definition_site),
            id: VariableId::LocalVariable(program.symbol_ids_mut().next_id()),
            base_field_id: None,
        }
    }

    /// Creates a new field with a given name and type.
    pub fn new_field(
        name: String,
        type_: Rc<RefCell<Type>>,
        definition_site: SourceOrigin,
        program: &mut Program,
    ) -> Variable {
        Variable {
            name,
            type_,
            definition_site: Some(definition_site),
            id: VariableId::Field(program.symbol_ids_mut().next_id()),
            base_field_id: None,
        }
    }

    /// Creates an internal function parameter from its proto-definition.
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

    /// Instantiates a field of a template base type, creating a copy for the instantiated type.
    ///
    /// A copy of this field is created with all occurrences of type parameters in its type
    /// replaced by concrete type arguments using `type_arguments` map.
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

    /// Instantiates a local variable of a template base function.
    ///
    /// A copy of this variable is created with all occurrences of type parameters in its type
    /// replaced by concrete type arguments using `type_arguments` map.
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

    /// Instantiates a parameter of an internal template base function.
    ///
    /// A copy of this parameter is created with all occurrences of type parameters in its type
    /// replaced by concrete type arguments using `type_arguments` map.
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

    /// Looks up the field instantiated from this field in an instantiated type.
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
