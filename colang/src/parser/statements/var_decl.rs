//! Variable declaration statement parser.

use crate::ast;
use crate::parser::equals::SingleEquals;
use crate::parser::ident::Identifier;
use crate::parser::prelude::*;
use crate::parser::stmt_or_expr::ExprLikeOrSynthesize;
use crate::parser::type_expr::TypeExprOrSynthesize;

pub struct VarDeclStmt;

impl Parser for VarDeclStmt {
    type N = ast::StmtOrExpr;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <Seq3<
            AbortIfMissing<WordParser<KwVar>>,
            VarDeclEntryOrSynthesize,
            AbortIfMissing<
                RepeatZeroOrMore<
                    Seq2<AbortIfMissing<CharsParser<Comma>>, VarDeclEntryOrSynthesize>,
                    DontRecover,
                >,
            >,
        >>::parse(input, ctx)
        .map(|(kw_var, head, tail)| {
            let span = kw_var + head.span + tail.last().map(|(_, e)| e.span);
            let mut entries = vec![head];
            for (_, entry) in tail {
                entries.push(entry);
            }
            ast::StmtOrExpr::VarDecl(ast::VarDeclStmt { span, entries })
        })
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
