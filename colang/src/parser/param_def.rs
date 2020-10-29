//! Parameter definition parser.

use crate::ast;
use crate::parser::normal_param_def::NormalParameterDef;
use crate::parser::prelude::*;

pub struct ParameterDef;

impl Parser for ParameterDef {
    type N = ast::Parameter;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        NormalParameterDef::parse(input, ctx)
    }
}
