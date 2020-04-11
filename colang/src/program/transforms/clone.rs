use crate::program::{Function, Variable};

pub fn clone_function(
    function: &Function,
    variable_map: impl Fn(&Variable) -> Variable,
) -> Function {
    unimplemented!()
}
