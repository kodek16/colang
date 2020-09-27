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
        ParsedNode::Recovered(_, errors) => Err(errors),
        ParsedNode::Missing(error) => Err(vec![error]),
    }
}

/// Node parsed by a parsing routine.
///
/// `Ok` case is the actual node produced by a successful parse. `Recovered` is produced
/// when the parser is sure that some part of the expected node was present and consumed,
/// but the parse was not fully successful. `Missing` appears when the expected node was not
/// found, and nothing was consumed.
enum ParsedNode<T> {
    Ok(T),
    Recovered(T, Vec<SyntaxError>),
    Missing(SyntaxError),
}

/// Result of running a parsing routine.
///
/// First element is the parsed node, second is the remainder of the input that was not consumed
/// by the parse.
struct ParseResult<'a, T>(ParsedNode<T>, Input<'a>);

/// Attempts to parse a node and exits immediately with `Missing` on failure.
///
/// This should be used for "anchor" nodes (e.g. keywords in the beginning of a production).
macro_rules! try_parse_or_abort {
    ($e: expr, $errors: expr) => {{
        let ParseResult(node, remaining) = $e;
        match node {
            ParsedNode::Ok(node) => (node, remaining),
            ParsedNode::Recovered(node, mut errors) => {
                $errors.append(&mut errors);
                (node, remaining)
            }
            ParsedNode::Missing(error) => {
                return ParseResult(ParsedNode::Missing(error), remaining);
            }
        }
    }};
}

/// Attempts to parse a node, falling back to a stub if not successful.
///
/// This effectively performs "recovery by insertion".
macro_rules! try_parse_or_synthesise {
    ($e: expr, $fallback: expr, $errors: expr) => {{
        let ParseResult(node, remaining) = $e;
        match node {
            ParsedNode::Ok(node) => (node, remaining),
            ParsedNode::Recovered(node, mut errors) => {
                $errors.append(&mut errors);
                (node, remaining)
            }
            ParsedNode::Missing(error) => {
                $errors.push(error);
                ($fallback, remaining)
            }
        }
    }};
}

/// Attempts to parse a node that is optional in the current position.
macro_rules! try_parse_opt {
    ($e: expr, $errors: expr) => {{
        let ParseResult(node, remaining) = $e;
        match node {
            ParsedNode::Ok(node) => (Some(node), remaining),
            ParsedNode::Recovered(node, mut errors) => {
                $errors.append(&mut errors);
                (Some(node), remaining)
            }
            ParsedNode::Missing(_) => (None, remaining),
        }
    }};
}

/// Completes a parse choosing an appropriate `ParsedNode` variant.
macro_rules! complete_parse {
    ($node: expr, $remaining: expr, $errors: expr) => {{
        let node = if ($errors.is_empty()) {
            ParsedNode::Ok($node)
        } else {
            ParsedNode::Recovered($node, $errors)
        };
        ParseResult(node, $remaining)
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
            ParsedNode::Recovered(function_def, mut es) => {
                functions.push(function_def);
                errors.append(&mut es);
                input = recover_to_next_global(input);
            }
            ParsedNode::Missing(e) => {
                errors.push(e);
                // TODO: this is probably useless as we are supposed to be in a good location?
                input = recover_to_next_global(input);
            }
        }
    }

    let program = ast::Program {
        functions,
        structs: vec![],
        traits: vec![],
    };
    complete_parse!(program, input, errors)
}

fn recover_to_next_global(input: Input) -> Input {
    let (_, remaining) = match GLOBAL_ANCHOR_RE.find(&input) {
        Some(match_) => input.split_at(match_.start()),
        None => input.consume_all(),
    };
    remaining
}

fn function_def<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, ast::FunctionDef> {
    let mut errors = vec![];

    let (fun_span, input) = try_parse_or_abort!(kw_fun(input, ctx), errors);
    let (name, input) = try_parse_or_synthesise!(
        ident(input, ctx),
        ast::Identifier {
            text: "<missing>".to_string(),
            span: input.span_of_first(ctx)
        },
        errors
    );
    let (_, input) = try_parse_or_synthesise!(
        chars_exact(input, ctx, "("),
        input.span_of_first(ctx),
        errors
    );
    let (_, input) = try_parse_or_synthesise!(
        chars_exact(input, ctx, ")"),
        input.span_of_first(ctx),
        errors
    );
    let (body, input) = try_parse_opt!(block(input, ctx), errors);

    let signature_span = fun_span + name.span;
    let function_def = ast::FunctionDef {
        name,
        parameters: vec![],
        return_type: None,
        body: body.map(ast::Expression::Block),
        signature_span,
    };
    complete_parse!(function_def, input, errors)
}

fn block<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, ast::BlockExpr> {
    let mut errors = vec![];

    let (left_brace_span, input) = try_parse_or_abort!(chars_exact(input, ctx, "{"), errors);
    let (right_brace_span, input) = try_parse_or_synthesise!(
        chars_exact(input, ctx, "}"),
        input.span_of_first(ctx),
        errors
    );

    let block = ast::BlockExpr {
        span: left_brace_span + right_brace_span,
        statements: vec![],
        final_expr: None,
    };
    complete_parse!(block, input, errors)
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
            ParseResult(ParsedNode::Missing(error), input)
        }
    }
}

fn kw_fun<'a>(input: Input<'a>, ctx: &ParsingContext) -> ParseResult<'a, InputSpan> {
    word_exact(input, ctx, "fun")
}

fn word_exact<'a>(
    input: Input<'a>,
    ctx: &ParsingContext,
    target_word: &str,
) -> ParseResult<'a, InputSpan> {
    let input = skip_ignored(input);

    let idx = input.find(|c: char| !c.is_alphanumeric());
    let (actual_word, remaining) = match idx {
        Some(idx) => input.split_at(idx),
        None => input.consume_all(),
    };

    if *actual_word == *target_word {
        ParseResult(ParsedNode::Ok(actual_word.span_of_full(ctx)), remaining)
    } else {
        // TODO maybe see if the word is similar, and if it is, eat it?
        let error = SyntaxError::UnexpectedToken(actual_word.span_of_first(ctx));
        ParseResult(ParsedNode::Missing(error), input)
    }
}

/// Consumes `target_chars`, expecting them to be the next non-whitespace characters on input.
///
/// Unlike `word_exact`, the characters do not have to be alphanumeric, and do not have to
/// be followed by a specific boundary.
fn chars_exact<'a>(
    input: Input<'a>,
    ctx: &ParsingContext,
    target_chars: &str,
) -> ParseResult<'a, InputSpan> {
    let input = skip_ignored(input);

    if &input[..target_chars.len()] == target_chars {
        let (chars, remaining) = input.split_at(target_chars.len());
        ParseResult(ParsedNode::Ok(chars.span_of_full(ctx)), remaining)
    } else {
        let error = SyntaxError::UnexpectedToken(input.span_of_first(ctx));
        ParseResult(ParsedNode::Missing(error), input)
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
