//! Parameter lists parser.

use crate::ast;
use crate::parser::comma_list::CommaSeparated;
use crate::parser::param_def::ParameterDef;
use crate::parser::prelude::*;

pub struct ParameterList;

impl Parser for ParameterList {
    type N = Vec<ast::Parameter>;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <CommaSeparated<ParameterDef>>::parse(input)
    }
}
