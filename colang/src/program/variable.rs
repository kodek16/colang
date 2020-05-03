//! Definitions and handling routines for CO variables.

use crate::program::function::ProtoInternalParameter;
use crate::program::{
    FunctionId, InternalFunctionTag, Program, SymbolId, Type, TypeId, TypeRegistry,
};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A variable in CO: a named, stateful value with a certain lifecycle.
///
/// Currently all variables in CO are _local_: their lifecycle is bound to evaluation of some
/// expression.
///
/// A local variable is a named "box" for a value of some determined type defined within
/// a function body (or more specifically, directly in a block expression).
///
/// Local variables instances (not in template sense) get created and destroyed automatically when
/// their enclosing block starts and finishes its execution.
///
/// Function parameters are a special kind of local variables: they are a part of a function
/// signature, and they automatically get assigned the value of the corresponding argument provided
/// at the function call site.
pub struct Variable {
    /// The name of the variable.
    pub name: String,

    /// A unique identifier of the variable that also includes information about its kind.
    pub id: VariableId,

    /// The location in program source code where the variable is defined.
    ///
    /// This can be `None` only for internal function parameters.
    pub definition_site: Option<SourceOrigin>,

    /// The type of the variable (or equivalently, the type of its value).
    pub type_: Rc<RefCell<Type>>,
}

/// A plain, hashable, unique identifier for variables.
///
/// Also includes information about what kind of variable a `Variable` represents exactly:
/// different variants correspond to different kinds.
#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub enum VariableId {
    /// Identifies a local variable defined in a function which has not been instantiated.
    LocalVariable(SymbolId),

    /// Identifies a local variable defined in an instantiated function.
    InstantiatedLocalVariable(SymbolId, FunctionId),

    /// Identifies a parameter of an internal function.
    ///
    /// Second element is the index of the parameter, starting from zero.
    InternalParameter(InternalFunctionTag, usize),
}

impl Variable {
    /// Creates a new variable with a given name and type.
    pub fn new(
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
            _ => panic!("Attempt to instantiate a variable that is not an internal parameter"),
        };

        Variable {
            id,
            name: self.name.clone(),
            definition_site: self.definition_site,
            type_: Type::substitute(&self.type_, type_arguments, types),
        }
    }
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
