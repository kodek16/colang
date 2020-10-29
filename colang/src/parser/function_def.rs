//! Parser for function definitions.

use crate::ast;
use crate::parser::block::Block;
use crate::parser::ident::Identifier;
use crate::parser::param_list::ParameterList;
use crate::parser::prelude::*;

pub struct FunctionDef;

impl Parser for FunctionDef {
    type N = ast::FunctionDef;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq6<
            AbortIfMissing<word::KwFun>,
            NameItem,
            LeftParenItem,
            ParameterListItem,
            RightParenItem,
            Optional<Block>,
        >>::parse(input, ctx)
        .map(
            |(kw_fun, name, _, parameters, paren_r, body)| ast::FunctionDef {
                signature_span: kw_fun + paren_r,
                name,
                parameters,
                return_type: None,
                body: body.map(ast::Expression::Block),
            },
        )
    }
}

struct NameItem;

impl SynthesizeIfMissing for NameItem {
    type P = Identifier;

    fn synthesize(location: InputSpan) -> ast::Identifier {
        ast::Identifier {
            text: "<missing>".to_string(),
            span: location,
        }
    }
}

struct LeftParenItem;

impl SynthesizeIfMissing for LeftParenItem {
    type P = chars::LeftParen;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}

struct ParameterListItem;

impl SynthesizeIfMissing for ParameterListItem {
    type P = ParameterList;

    fn synthesize(_: InputSpan) -> Vec<ast::Parameter> {
        vec![]
    }
}

struct RightParenItem;

impl SynthesizeIfMissing for RightParenItem {
    type P = chars::RightParen;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}
