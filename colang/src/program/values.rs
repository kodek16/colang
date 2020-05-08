//! Value categories definitions.

/// The value category in CO determines what are the operations supported for a certain value.
///
/// Every value, and also every expression in CO has a determined value category.
///
/// There are only two value categories: rvalue and lvalue. Rvalues are much more common, and
/// less powerful: every lvalue can be used as an rvalue, but not the other way around. Lvalues
/// are values that must have an address an memory. Only lvalues support addressing, assignment,
/// and similar operations.
///
/// Arguably the most common kind of lvalue expression is a variable reference.
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ValueCategory {
    /// A value with an address in memory.
    Lvalue,

    /// A value potentially without an address in memory.
    Rvalue,
}
