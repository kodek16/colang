use crate::errors::CompilationError;
use crate::program::Expression;
use crate::typing::TypeRegistry;

/// Convenience function for checking condition type.
pub fn check_condition_is_bool(
    condition: &Expression,
    types: &TypeRegistry,
) -> Result<(), CompilationError> {
    let cond_type = &condition.type_;
    if *cond_type != *types.bool() {
        let error = CompilationError::condition_is_not_bool(
            cond_type.borrow().name(),
            condition
                .span
                .expect("Generated condition expression is not bool"),
        );
        Err(error)
    } else {
        Ok(())
    }
}

pub fn check_operand_is_int(
    operand: &Expression,
    types: &TypeRegistry,
) -> Result<(), CompilationError> {
    let operand_type = &operand.type_;
    if *operand_type != *types.int() {
        let error = CompilationError::operand_is_not_int(
            operand_type.borrow().name(),
            operand.span.expect("Generated operand is not int"),
        );
        Err(error)
    } else {
        Ok(())
    }
}
