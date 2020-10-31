//! Reusable parsers for low-level nodes.

use crate::parser::common::{ParseResult, ParsedNode, Parser, SyntaxError};
use crate::parser::context::ParsingContext;
use crate::parser::input::Input;
use crate::source::InputSpan;
use paste::paste;
use std::marker::PhantomData;

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

/// Consumes `$target`, expecting them to be the next non-whitespace characters on input.
///
/// Unlike `word_parser`, the characters do not have to be alphanumeric, and do not have to
/// be followed by a specific boundary.
macro_rules! chars_parser {
    ($name: ident, $target: literal) => {
        paste! {
            #[allow(non_snake_case)]
            mod [<$name _internal>] {
                use super::*;

                struct Internal;

                impl Parser for Internal {
                    type N = InputSpan;

                    fn parse<'a>(
                        input: Input<'a>,
                        ctx: &ParsingContext,
                    ) -> ParseResult<'a, Self::N> {
                        if &input[..$target.len()] == $target {
                            let (chars, remaining) = input.split_at($target.len());
                            ParseResult(ParsedNode::Ok(chars.span_of_full(ctx)), remaining)
                        } else {
                            let error = SyntaxError::UnexpectedToken(input.span_of_first(ctx));
                            ParseResult(ParsedNode::Missing(error), input)
                        }
                    }
                }

                pub struct $name;

                impl Parser for $name {
                    type N = InputSpan;

                    fn parse<'a>(
                        input: Input<'a>,
                        ctx: &ParsingContext,
                    ) -> ParseResult<'a, InputSpan> {
                        <WithIgnored<Internal>>::parse(input, ctx)
                    }
                }
            }

            pub use [<$name _internal>]::$name;
        }
    };
}

// Because of how macros are exported it would be inconvenient to keep the instantiations in
// their respective modules.

pub mod chars {
    use super::*;

    chars_parser!(LeftBrace, "{");
    chars_parser!(RightBrace, "}");
    chars_parser!(LeftParen, "(");
    chars_parser!(RightParen, ")");
    chars_parser!(Comma, ",");
    chars_parser!(Colon, ":");
}

/// Consumes the next full word if it matches the expected word (`$target`).
///
/// Unlike `chars_parser`, the word has to contain only alphanumeric characters, and must end
/// with a word boundary (e.g. `word_parser!(_, "fun")` would not match `"fund"`.
macro_rules! word_parser {
    ($name: ident, $target: literal) => {
        paste! {
            #[allow(non_snake_case)]
            mod [<$name _internal>] {
                use super::*;

                struct Internal;

                impl Parser for Internal {
                    type N = InputSpan;

                    fn parse<'a>(
                        input: Input<'a>,
                        ctx: &ParsingContext,
                    ) -> ParseResult<'a, Self::N> {
                        let idx = input.find(|c: char| !c.is_alphanumeric());
                        let (actual_word, remaining) = match idx {
                            Some(idx) => input.split_at(idx),
                            None => input.consume_all(),
                        };

                        if *actual_word == *$target {
                            ParseResult(ParsedNode::Ok(actual_word.span_of_full(ctx)), remaining)
                        } else {
                            // TODO maybe see if the word is similar, and if it is, recover?
                            let error =
                                SyntaxError::UnexpectedToken(actual_word.span_of_first(ctx));
                            ParseResult(ParsedNode::Missing(error), input)
                        }
                    }
                }

                pub struct $name;

                impl Parser for $name {
                    type N = InputSpan;

                    fn parse<'a>(
                        input: Input<'a>,
                        ctx: &ParsingContext,
                    ) -> ParseResult<'a, Self::N> {
                        <WithIgnored<Internal>>::parse(input, ctx)
                    }
                }
            }

            pub use [<$name _internal>]::$name;
        }
    };
}

pub mod word {
    use super::*;

    word_parser!(KwFun, "fun");
}

// Some other modules, like `ident` and `expressions::int_literal` could use the same pattern,
// having a "regex_parser" macro here. For now I decided not to do this because it's much more
// likely that regex "terminals" could have idiosyncratic behavior. When const-generics land,
// we could rework this entire idea with macros using modules parametrized with strings (ideally)
// or at least integer ids pointing to a global static table.
