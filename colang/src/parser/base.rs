//! Reusable parsers for low-level nodes.

use std::marker::PhantomData;

use crate::parser::common::{ParseResult, ParsedNode, Parser, SyntaxError};
use crate::parser::context::ParsingContext;
use crate::parser::input::Input;
use crate::source::InputSpan;

/// Skips comments and whitespace.
pub struct Ignored;

impl Parser for Ignored {
    type N = ();

    fn parse<'a>(mut input: Input<'a>, _: &ParsingContext) -> ParseResult<'a, Self::N> {
        loop {
            if input.starts_with(char::is_whitespace) {
                let (_, remaining) = match input.find(|c: char| !c.is_whitespace()) {
                    Some(idx) => input.split_at(idx),
                    None => input.consume_all(),
                };
                input = remaining;
                continue;
            }
            if input.starts_with("//") {
                let (_, remaining) = match input.find('\n') {
                    Some(idx) => input.split_at(idx),
                    None => input.consume_all(),
                };
                input = remaining;
                continue;
            }
            break;
        }
        ParseResult(ParsedNode::Ok(()), input)
    }
}

/// Wraps another parser, making sure that ignored tokens are discarded before the parse.
///
/// This should be used with most parsers, unless they handle ignored tokens in a non-trivial
/// way.
///
/// Notably, there is no need to use `WithIgnored` with `SeqN` parsers as they automatically
/// wrap their items in `WithIgnored`.
pub struct WithIgnored<P: Parser> {
    phantom: PhantomData<P>,
}

impl<P: Parser> Parser for WithIgnored<P> {
    type N = P::N;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        let ParseResult(result, input) = Ignored::parse(input, ctx);
        assert!(result.is_ok());
        P::parse(input, ctx)
    }
}

pub trait Chars {
    const CHARS: &'static str;
}

/// Consumes `C`, expecting them to be the next non-whitespace characters on input.
///
/// Unlike `WordParser`, the characters do not have to be alphanumeric, and do not have to
/// be followed by a specific boundary.
pub struct CharsParser<C: Chars> {
    phantom: PhantomData<C>,
}

impl<C: Chars> Parser for CharsParser<C> {
    type N = InputSpan;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <WithIgnored<CharsParserInternal<C>>>::parse(input, ctx)
    }
}

pub struct CharsParserInternal<C: Chars> {
    phantom: PhantomData<C>,
}

impl<C: Chars> Parser for CharsParserInternal<C> {
    type N = InputSpan;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        if &input[..C::CHARS.len()] == C::CHARS {
            let (chars, remaining) = input.split_at(C::CHARS.len());
            ParseResult(ParsedNode::Ok(chars.span_of_full(ctx)), remaining)
        } else {
            let error = SyntaxError::UnexpectedToken(input.span_of_first(ctx));
            ParseResult(ParsedNode::Missing(error), input)
        }
    }
}

pub trait Word {
    const WORD: &'static str;
}

/// Consumes the next full word if it matches the expected word.
///
/// Unlike `CharsParser`, the word has to contain only alphanumeric characters, and must end
/// with a word boundary (e.g. `WordParser<KwFun>` would not match `"fund"`.
pub struct WordParser<W: Word> {
    phantom: PhantomData<W>,
}

impl<W: Word> Parser for WordParser<W> {
    type N = InputSpan;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        <WithIgnored<WordParserInternal<W>>>::parse(input, ctx)
    }
}

struct WordParserInternal<W: Word> {
    phantom: PhantomData<W>,
}

impl<W: Word> Parser for WordParserInternal<W> {
    type N = InputSpan;

    fn parse<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, Self::N> {
        let idx = input.find(|c: char| !c.is_alphanumeric());
        let (actual_word, remaining) = match idx {
            Some(idx) => input.split_at(idx),
            None => input.consume_all(),
        };

        if *actual_word == *W::WORD {
            ParseResult(ParsedNode::Ok(actual_word.span_of_full(ctx)), remaining)
        } else {
            // TODO maybe see if the word is similar, and if it is, recover?
            let error = SyntaxError::UnexpectedToken(actual_word.span_of_first(ctx));
            ParseResult(ParsedNode::Missing(error), input)
        }
    }
}

// Some other modules, like `ident` and `expressions::int_literal` could use the same pattern,
// having a `RegexParser` generic struct here. This seems harder to do because of lazy_static!
// use. Regex terminals also seem more likely to have some unique behavior for every instance,
// so `RegexParser` might be less justified.
