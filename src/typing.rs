//! CO types and their properties are defined in this module.

use std::cell::RefCell;
use std::rc::Rc;

// Numeric types plan:
// int, int8, int16, int64, int128
// float, double

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,

    /// An invalid type. It can never appear in a valid program.
    Error,
}

impl Type {
    /// A convenience method for constructing managed error type instances.
    pub fn error() -> Rc<RefCell<Type>> {
        Rc::new(RefCell::new(Type::Error))
    }

    pub fn name(&self) -> String {
        use Type::*;
        match self {
            Int => "int".to_string(),
            Bool => "bool".to_string(),

            Error => "<error>".to_string(),
        }
    }

    pub fn is_error(&self) -> bool {
        match self {
            Type::Error => true,
            _ => false,
        }
    }
}
