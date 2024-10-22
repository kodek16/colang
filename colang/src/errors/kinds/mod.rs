//! Compilation error kinds.

mod e0001_syntax_error;
mod e0002_named_entity_not_found;
mod e0003_named_entity_kind_mismatch;
mod e0004_named_entity_already_defined;
mod e0005_condition_not_bool;
mod e0006_array_size_not_int;
mod e0008_deref_operand_not_pointer;
mod e0009_read_unsupported_type;
mod e0010_readln_unsupported_type;
mod e0011_write_value_not_stringable;
mod e0013_variable_no_type_or_initializer;
mod e0015_return_value_in_void_function;
mod e0016_return_no_value_in_non_void_function;
mod e0018_assignment_type_mismatch;
mod e0019_variable_initializer_type_mismatch;
mod e0020_function_body_type_mismatch;
mod e0021_return_statement_type_mismatch;
mod e0022_if_expression_branch_type_mismatch;
mod e0023_is_expression_type_mismatch;
mod e0024_call_argument_type_mismatch;
mod e0025_array_elements_type_mismatch;
mod e0026_call_wrong_number_of_arguments;
mod e0027_wrong_number_of_type_template_arguments;
mod e0028_assignment_target_not_lvalue;
mod e0029_read_target_not_lvalue;
mod e0030_address_of_rvalue;
mod e0031_cannot_infer_empty_array_type;
mod e0032_cannot_infer_null_pointer_type;
mod e0033_self_must_be_lvalue;
mod e0034_self_not_in_method_signature;
mod e0035_self_in_function_body;
mod e0036_self_is_not_first_parameter;
mod e0037_method_first_parameter_is_not_self;
mod e0038_literal_not_utf8;
mod e0039_char_literal_bad_length;
mod e0040_unknown_escape_sequence;
mod e0041_index_method_returns_not_pointer;
mod e0042_binary_operator_unsupported_types;
mod e0043_logical_operator_operand_wrong_type;
mod e0044_is_expression_operand_wrong_type;
mod e0045_type_infinite_dependency_chain;
mod e0046_function_infinite_dependency_chain;
mod e0047_type_cycle_through_fields;
mod e0048_main_function_not_found;
mod e0049_function_body_missing;
mod e0050_field_in_trait;
mod e0051_method_with_body_in_trait;
mod e0052_type_used_as_trait;
mod e0053_trait_method_signature_mismatch;
mod e0054_trait_method_not_implemented;
mod e0055_conflicting_method_from_trait_bounds;
mod e0056_type_argument_violates_trait_bound;
mod e0057_statement_used_as_expression;
mod e0058_non_void_function_body_is_statement;

pub mod constructors {
    use super::*;

    pub use e0001_syntax_error::syntax_error;
    pub use e0002_named_entity_not_found::named_entity_not_found;
    pub use e0003_named_entity_kind_mismatch::named_entity_kind_mismatch;
    pub use e0004_named_entity_already_defined::named_entity_already_defined;
    pub use e0005_condition_not_bool::condition_not_bool;
    pub use e0006_array_size_not_int::array_size_not_int;
    pub use e0008_deref_operand_not_pointer::deref_operand_not_pointer;
    pub use e0009_read_unsupported_type::read_unsupported_type;
    pub use e0010_readln_unsupported_type::readln_unsupported_type;
    pub use e0011_write_value_not_stringable::write_value_not_stringable;
    pub use e0013_variable_no_type_or_initializer::variable_no_type_or_initializer;
    pub use e0015_return_value_in_void_function::return_value_in_void_function;
    pub use e0016_return_no_value_in_non_void_function::return_no_value_in_non_void_function;
    pub use e0018_assignment_type_mismatch::assignment_type_mismatch;
    pub use e0019_variable_initializer_type_mismatch::variable_initializer_type_mismatch;
    pub use e0020_function_body_type_mismatch::function_body_type_mismatch;
    pub use e0021_return_statement_type_mismatch::return_statement_type_mismatch;
    pub use e0022_if_expression_branch_type_mismatch::if_expression_branch_type_mismatch;
    pub use e0023_is_expression_type_mismatch::is_expression_type_mismatch;
    pub use e0024_call_argument_type_mismatch::call_argument_type_mismatch;
    pub use e0025_array_elements_type_mismatch::array_elements_type_mismatch;
    pub use e0026_call_wrong_number_of_arguments::call_wrong_number_of_arguments;
    pub use e0027_wrong_number_of_type_template_arguments::wrong_number_of_type_template_arguments;
    pub use e0028_assignment_target_not_lvalue::assignment_target_not_lvalue;
    pub use e0029_read_target_not_lvalue::read_target_not_lvalue;
    pub use e0030_address_of_rvalue::address_of_rvalue;
    pub use e0031_cannot_infer_empty_array_type::cannot_infer_empty_array_type;
    pub use e0032_cannot_infer_null_pointer_type::cannot_infer_null_pointer_type;
    pub use e0033_self_must_be_lvalue::self_must_be_lvalue;
    pub use e0034_self_not_in_method_signature::self_not_in_method_signature;
    pub use e0035_self_in_function_body::self_in_function_body;
    pub use e0036_self_is_not_first_parameter::self_is_not_first_parameter;
    pub use e0037_method_first_parameter_is_not_self::method_first_parameter_is_not_self;
    pub use e0038_literal_not_utf8::literal_not_utf8;
    pub use e0039_char_literal_bad_length::char_literal_bad_length;
    pub use e0040_unknown_escape_sequence::unknown_escape_sequence;
    pub use e0041_index_method_returns_not_pointer::index_method_returns_not_pointer;
    pub use e0042_binary_operator_unsupported_types::binary_operator_unsupported_types;
    pub use e0043_logical_operator_operand_wrong_type::logical_operator_operand_wrong_type;
    pub use e0044_is_expression_operand_wrong_type::is_expression_operand_wrong_type;
    pub use e0045_type_infinite_dependency_chain::type_infinite_dependency_chain;
    pub use e0046_function_infinite_dependency_chain::function_infinite_dependency_chain;
    pub use e0047_type_cycle_through_fields::type_cycle_through_fields;
    pub use e0048_main_function_not_found::main_function_not_found;
    pub use e0049_function_body_missing::function_body_missing;
    pub use e0050_field_in_trait::field_in_trait;
    pub use e0051_method_with_body_in_trait::method_with_body_in_trait;
    pub use e0052_type_used_as_trait::type_used_as_trait;
    pub use e0053_trait_method_signature_mismatch::trait_method_signature_mismatch;
    pub use e0054_trait_method_not_implemented::trait_method_not_implemented;
    pub use e0055_conflicting_method_from_trait_bounds::conflicting_method_from_trait_bounds;
    pub use e0056_type_argument_violates_trait_bound::type_argument_violates_trait_bound;
    pub use e0057_statement_used_as_expression::statement_used_as_expression;
    pub use e0058_non_void_function_body_is_statement::non_void_function_body_is_statement;
}
