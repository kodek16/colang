//! Top-level parser for the entire program.

use crate::ast;
use crate::parser::function_def::FunctionDef;
use crate::parser::prelude::*;
use crate::parser::repeat::RecoverToToken;
use crate::parser::tokens::primary::{PrimaryToken, PrimaryTokenPayload};

pub struct Program;

impl Parser for Program {
    type N = ast::Program;

    fn parse(input: Input) -> ParseResult<Self::N> {
        let ParseResult(node, input) =
            <RepeatZeroOrMore<FunctionDef, RecoverToNextGlobal>>::parse(input).map(|functions| {
                ast::Program {
                    functions,
                    structs: vec![],
                    traits: vec![],
                }
            });

        match input.with_primary_tokenizer().peek() {
            Some(token) => {
                let error = SyntaxError::UnexpectedToken(token.span);
                ParseResult(node, input).add_error(error)
            }
            None => ParseResult(node, input),
        }
    }
}

struct RecoverToNextGlobal;

impl RecoverToToken for RecoverToNextGlobal {
    fn is_anchor(token: PrimaryToken) -> bool {
        match token.payload {
            PrimaryTokenPayload::KwFun => true,
            _ => false,
        }
    }
}
