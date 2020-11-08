//! Variable declaration statement parser.

use crate::ast;
use crate::parser::equals::SingleEquals;
use crate::parser::ident::Identifier;
use crate::parser::prelude::*;
use crate::parser::stmt_or_expr::ExprLikeOrSynthesize;
use crate::parser::type_expr::TypeExprOrSynthesize;

pub struct VarDeclStmt;

impl Parser for VarDeclStmt {
    type N = ast::VarDeclStmt;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq2<AbortIfMissing<WordParser<KwVar>>, VarDeclEntryOrSynthesize>>::parse(input, ctx).map(
            |(kw_var, entry)| {
                let span = kw_var + entry.span;
                ast::VarDeclStmt {
                    span,
                    entries: vec![entry],
                }
            },
        )
    }
}

struct VarDeclEntry;

impl Parser for VarDeclEntry {
    type N = ast::VarDeclEntry;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq3<
            AbortIfMissing<Identifier>,
            Optional<Seq2<AbortIfMissing<CharsParser<Colon>>, TypeExprOrSynthesize>>,
            Optional<Seq2<AbortIfMissing<SingleEquals>, ExprLikeOrSynthesize>>,
        >>::parse(input, ctx)
        .map(|(name, type_, initializer)| {
            let type_ = type_.map(|(_, t)| t);
            let initializer = initializer.map(|(_, i)| i);
            let span = name.span
                + type_.as_ref().map(|t| t.span())
                + initializer.as_ref().map(|i| i.span());
            ast::VarDeclEntry {
                span,
                variable_name: name,
                variable_type: type_,
                initializer,
            }
        })
    }
}

struct VarDeclEntryOrSynthesize;

impl SynthesizeIfMissing for VarDeclEntryOrSynthesize {
    type P = VarDeclEntry;

    fn synthesize(location: InputSpan) -> ast::VarDeclEntry {
        ast::VarDeclEntry {
            span: location,
            variable_name: ast::Identifier {
                text: "<missing>".to_string(),
                span: location,
            },
            variable_type: None,
            initializer: None,
        }
    }
}
