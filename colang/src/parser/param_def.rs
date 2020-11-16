//! Parameter definition parser.

use crate::ast;
use crate::parser::normal_param_def::NormalParameterDef;
use crate::parser::prelude::*;

pub struct ParameterDef;

impl Parser for ParameterDef {
    type N = ast::Parameter;

    fn parse(input: Input) -> ParseResult<Self::N> {
        NormalParameterDef::parse(input)
    }
}
