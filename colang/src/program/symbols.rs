//! Symbol IDs handling.

/// A unique identifier assigned to every named entity defined in the program.
///
/// Symbol IDs are assigned to user-defined variables, functions, types, etc., and are unique
/// across all kinds of named entities.
pub type SymbolId = u32;

/// A registry that keeps track of assigned and free symbol IDs.
pub struct SymbolIdRegistry {
    next_id: SymbolId,
}

impl SymbolIdRegistry {
    pub fn new() -> SymbolIdRegistry {
        SymbolIdRegistry { next_id: 1 }
    }

    /// Acquires a new unique ID from the registry.
    ///
    /// This ID is guaranteed to be never returned on a subsequent call to `next_id`.
    pub fn next_id(&mut self) -> SymbolId {
        let result = self.next_id;
        self.next_id += 1;
        result
    }
}
