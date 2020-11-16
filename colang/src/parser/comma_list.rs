//! Comma-separated lists parser.
//!
//! This parser is very conservative, and will only consume input until the first "structural"
//! error.

use std::marker::PhantomData;

use crate::parser::prelude::*;

pub struct CommaSeparated<P: Parser> {
    phantom: PhantomData<P>,
}

impl<P: Parser> Parser for CommaSeparated<P> {
    type N = Vec<P::N>;

    fn parse(input: Input) -> ParseResult<Self::N> {
        <Seq2<
            Optional<RepeatZeroOrMore<Seq2<AbortIfMissing<P>, AbortIfMissing<Comma>>, DontRecover>>,
            Optional<P>,
        >>::parse(input)
        .map(|(seq, last)| {
            let mut result = Vec::new();
            if let Some(seq) = seq {
                result.extend(seq.into_iter().map(|(item, _)| item));
            }
            if let Some(last) = last {
                result.push(last);
            }
            result
        })
    }
}
