//! Parser for function definitions.

use crate::ast;
use crate::parser::block::Block;
use crate::parser::ident::Identifier;
use crate::parser::param_list::ParameterList;
use crate::parser::prelude::*;
use crate::parser::type_expr::TypeExprOrSynthesize;

pub struct FunctionDef;

impl Parser for FunctionDef {
    type N = ast::FunctionDef;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq7<
            AbortIfMissing<WordParser<KwFun>>,
            NameItem,
            LeftParenItem,
            ParameterListItem,
            RightParenItem,
            Optional<Seq2<AbortIfMissing<CharsParser<Colon>>, TypeExprOrSynthesize>>,
            Optional<Block>,
        >>::parse(input, ctx)
        .map(
            |(kw_fun, name, _, parameters, paren_r, return_type, body)| {
                let signature_span = kw_fun
                    + if let Some((_, ref type_)) = return_type {
                        type_.span()
                    } else {
                        paren_r
                    };
                ast::FunctionDef {
                    signature_span,
                    name,
                    parameters,
                    return_type: return_type.map(|(_, type_)| type_),
                    body: body.map(ast::ExpressionLike::Block),
                }
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
    type P = CharsParser<LeftParen>;

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
    type P = CharsParser<RightParen>;

    fn synthesize(location: InputSpan) -> InputSpan {
        location
    }
}
