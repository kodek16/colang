//! Function signature representation for diagnostics.

use crate::program::{Type, TypeId, TypeRegistry};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

/// A function signature object for use in diagnostic messages.
pub struct Signature {
    /// Function parameter types.
    pub parameters: Vec<Rc<RefCell<Type>>>,

    /// Function return type.
    pub return_type: Option<Rc<RefCell<Type>>>,
}

impl Signature {
    /// Substitutes some set of types in the signature with other types.
    ///
    /// Type substitutions are provided as a `HashMap` where keys are "from" type IDs, and values
    /// are "to" type IDs.
    pub fn substitute(
        &self,
        substitutions: &HashMap<TypeId, TypeId>,
        types: &mut TypeRegistry,
    ) -> Signature {
        Signature {
            parameters: self
                .parameters
                .iter()
                .map(|parameter_type| Type::substitute(parameter_type, substitutions, types))
                .collect(),
            return_type: self
                .return_type
                .as_ref()
                .map(|return_type| Type::substitute(return_type, substitutions, types)),
        }
    }
}

impl PartialEq for Signature {
    fn eq(&self, other: &Self) -> bool {
        self.parameters == other.parameters && self.return_type == other.return_type
    }
}

impl Display for Signature {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.parameters.is_empty() {
            write!(f, "()")?
        } else {
            write!(
                f,
                "{}",
                self.parameters
                    .iter()
                    .map(|parameter_type| parameter_type.borrow().name.clone())
                    .collect::<Vec<_>>()
                    .join(", ")
            )?
        }

        if let Some(ref return_type) = self.return_type {
            write!(f, " -> {}", return_type.borrow().name)?;
        }

        Ok(())
    }
}
