mod context;
mod input;

use crate::ast;
use crate::parser::context::ParsingContext;
use crate::parser::input::Input;
use crate::source::{InputSpan, InputSpanFile};
use lazy_static::lazy_static;
use regex::Regex;

/// A syntax error found in the user program.
#[derive(Debug)]
pub enum SyntaxError {
    UnexpectedToken(InputSpan),
}

/// Parses an entire source file, constructing an `ast::Program`.
pub fn parse(source_code: &str, file: InputSpanFile) -> Result<ast::Program, Vec<SyntaxError>> {
    let ctx = ParsingContext { file };
    let input = Input::new(source_code);

    let ParseResult(program, _) = program(input, &ctx);
    match program {
        ParsedNode::Ok(program) => Ok(program),
        ParsedNode::BestGuess(_, errors) => Err(errors),
    }
}

/// Node parsed by a parsing routine.
///
/// `Ok` case is the actual node produced by a successful parse. The other case
/// is a best-effort guess (or flat out stub) of what a successful parse could be.
enum ParsedNode<T> {
    Ok(T),
    BestGuess(T, Vec<SyntaxError>),
}

/// Result of running a parsing routine.
///
/// First element is the parsed node, second is the remainder of the input
/// that was not touched by the parse.
struct ParseResult<'a, T>(ParsedNode<T>, Input<'a>);

// Attempts to parse a sub-node and terminates immediately on failure.
// All consumed input (including this call) is still considered consumed.
// Guess is made based on the argument only, not using the sub-node guess.
macro_rules! try_parse {
    ($e: expr, $guess: expr) => {{
        let ParseResult(node, remaining) = $e;
        match node {
            ParsedNode::Ok(node) => (node, remaining),
            ParsedNode::BestGuess(_, errors) => {
                return ParseResult(ParsedNode::BestGuess($guess, errors), remaining);
            }
        }
    }};
}

// Cannot have this locally inside functions because of an intellij-rust bug.
// (it works in pure Cargo though).
//
// Tracking issue: https://github.com/intellij-rust/intellij-rust/issues/6164
lazy_static! {
    static ref GLOBAL_ANCHOR_RE: Regex = Regex::new(r"\b(fun|struct|trait)\b").unwrap();
    static ref IDENT_RE: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
}

fn program<'a>(mut input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, ast::Program> {
    let mut functions = vec![];
    let mut errors = vec![];
    while !input.is_fully_consumed() {
        let ParseResult(function_def, remaining) = function_def(input, ctx);
        input = remaining;

        match function_def {
            ParsedNode::Ok(function_def) => functions.push(function_def),
            ParsedNode::BestGuess(_, mut es) => {
                errors.append(&mut es);
                input = recover_to_next_global(input);
            }
        }
    }

    let program = ast::Program {
        functions,
        structs: vec![],
        traits: vec![],
    };

    let node = if errors.is_empty() {
        ParsedNode::Ok(program)
    } else {
        ParsedNode::BestGuess(program, errors)
    };

    ParseResult(node, input)
}

fn recover_to_next_global(input: Input) -> Input {
    let (_, remaining) = match GLOBAL_ANCHOR_RE.find(&input) {
        Some(match_) => input.split_at(match_.start()),
        None => input.consume_all(),
    };
    remaining
}

fn function_def<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, ast::FunctionDef> {
    let guess = ast::FunctionDef {
        name: ast::Identifier {
            text: "<error>".to_string(),
            span: input.span_of_first(ctx),
        },
        parameters: vec![],
        return_type: None,
        body: None,
        signature_span: input.span_of_first(ctx),
    };

    let signature_begin_span = input.span_of_first(ctx);
    let ((), input) = try_parse!(kw_fun(input, ctx), guess);
    let (name, input) = try_parse!(ident(input, ctx), guess);

    let signature_span = signature_begin_span + name.span;
    let function_def = ast::FunctionDef {
        name,
        parameters: vec![],
        return_type: None,
        body: None,
        signature_span,
    };

    ParseResult(ParsedNode::Ok(function_def), input)
}

fn ident<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, ast::Identifier> {
    let input = skip_ignored(input);

    match input.consume_regex_if_matches(&IDENT_RE) {
        Some((word, remaining)) => {
            let identifier = ast::Identifier {
                text: word.to_string(),
                span: word.span_of_full(ctx),
            };
            ParseResult(ParsedNode::Ok(identifier), remaining)
        }
        None => {
            let error = SyntaxError::UnexpectedToken(input.span_of_first(ctx));
            let guess = ast::Identifier {
                text: String::from("<error>"),
                span: input.span_of_first(ctx),
            };
            ParseResult(ParsedNode::BestGuess(guess, vec![error]), input)
        }
    }
}

fn kw_fun<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, ()> {
    word_exact(input, ctx, "fun")
}

fn word_exact<'a>(
    input: Input<'a>,
    ctx: &ParsingContext,
    target_word: &str,
) -> ParseResult<'a, ()> {
    let input = skip_ignored(input);

    let idx = input.find(|c: char| !c.is_alphanumeric());
    let (actual_word, remaining) = match idx {
        Some(idx) => input.split_at(idx),
        None => input.consume_all(),
    };

    if *actual_word == *target_word {
        ParseResult(ParsedNode::Ok(()), remaining)
    } else {
        // TODO maybe see if the word is similar, and if it is, eat it?
        let error = SyntaxError::UnexpectedToken(actual_word.span_of_first(ctx));
        ParseResult(ParsedNode::BestGuess((), vec![error]), input)
    }
}

fn skip_ignored(input: Input) -> Input {
    // TODO: also skip comments.
    let idx = input.find(|c: char| !c.is_whitespace());
    let (_, remaining) = match idx {
        Some(idx) => input.split_at(idx),
        None => input.consume_all(),
    };
    remaining
}
