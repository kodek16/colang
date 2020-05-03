//! Definitions and routines for working with CO struct fields.

use crate::program::{Program, SymbolId, Type, TypeId, TypeRegistry};
use crate::source::SourceOrigin;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// A field of a CO struct: named, individually referencable part of a struct value.
///
/// Every struct type has a separate set of fields, and its value is a product of
/// values of all of its fields.
pub struct Field {
    /// The name of the field.
    ///
    /// Names of all fields in a struct type must be unique. This is ensured by its
    /// associated `TypeScope`.
    pub name: String,

    /// A unique, hashable identifier of the field.
    pub id: FieldId,

    /// The location in program source code where the field is defined.
    pub definition_site: SourceOrigin,

    /// The type of the field.
    pub type_: Rc<RefCell<Type>>,

    /// For instantiated fields, this is the ID of their prototype field in the base type.
    base_field_id: Option<FieldId>,
}

/// A plain, hashable, unique identifier for fields.
///
/// Also includes information about whether the field belongs to a type instantiated
/// from a template (and so has a corresponding "base field" in the base type).
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub enum FieldId {
    Plain(SymbolId),
    Instantiated(SymbolId, TypeId),
}

impl Field {
    /// Creates a new field with a given name and type.
    pub fn new(
        name: String,
        type_: Rc<RefCell<Type>>,
        definition_site: SourceOrigin,
        program: &mut Program,
    ) -> Field {
        Field {
            name,
            type_,
            definition_site,
            id: FieldId::Plain(program.symbol_ids_mut().next_id()),
            base_field_id: None,
        }
    }

    /// Instantiates a field of a template base type, creating a copy for the instantiated type.
    ///
    /// A copy of this field is created with all occurrences of type parameters in its type
    /// replaced by concrete type arguments using `type_arguments` map.
    pub fn instantiate(
        &self,
        instantiated_type_id: TypeId,
        type_arguments: &HashMap<TypeId, TypeId>,
        types: &mut TypeRegistry,
    ) -> Field {
        let id = match self.id {
            FieldId::Plain(id) => FieldId::Instantiated(id, instantiated_type_id),
            FieldId::Instantiated(_, _) => {
                panic!("Attempt to further instantiate an already instantiated field")
            }
        };

        Field {
            id,
            name: self.name.clone(),
            definition_site: self.definition_site,
            type_: Type::substitute(&self.type_, type_arguments, types),
            base_field_id: Some(self.id.clone()),
        }
    }

    /// Looks up the field of the same origin as this field in an instantiated type.
    ///
    /// This method works both for fields of base types (finding the field instantiated from
    /// this base field), and for other instantiated types (finding the field instantiated from
    /// the same base field as this field).
    pub fn lookup_instantiated_field(&self, instantiated_type: &Type) -> Rc<RefCell<Field>> {
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

impl PartialEq for Field {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
