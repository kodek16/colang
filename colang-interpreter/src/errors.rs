//! Runtime error and stack traces handling.

use crate::values::{Rvalue, Value};
use crate::EarlyExit;
use colang::program::{CallExpr, Function};
use colang::source::{InputSpanFile, SourceOrigin};
use std::cell::RefCell;
use std::rc::Rc;

/// An error caused by user that occurred while executing the program.
///
/// Once a runtime error is detected, normal program execution stops, and stack unwinding begins.
/// During stack unwinding, the interpreter backtracks through the function call stack leading to
/// the error, and attaches the stack information to the `RuntimeError` object.
pub struct RuntimeError {
    /// The error message that is displayed to the user.
    message: String,

    /// The function call stack leading to the error.
    ///
    /// The first element is the innermost call, the last element is the outermost.
    call_stack: Vec<StackFrame>,

    /// A call location in the stack not yet bound to a stack frame.
    ///
    /// When the information about the outer function call is provided through
    /// `annotate_stack_frame`, it will be joined with this location into a new stack frame on
    /// `call_stack`.
    bottom_location: Option<SourceOrigin>,
}

impl RuntimeError {
    /// Creates a new runtime error.
    ///
    /// Location of the causing expression or statement must be given most of the time, the only
    /// exception is when the error occurred in an internal function call (e.g. through a contract
    /// violation).
    pub fn new(message: impl Into<String>, location: Option<SourceOrigin>) -> EarlyExit {
        EarlyExit::Error(RuntimeError {
            message: message.into(),
            call_stack: Vec::new(),
            bottom_location: location,
        })
    }

    /// Appends a new stack frame during the unwinding process.
    ///
    /// The function called in the `CallExpr` is added as a frame, including information about
    /// the values of the arguments at the time of call, and the location reached inside the
    /// call (which is stored in `bottom_location`).
    pub fn annotate_stack_frame(mut self, call: &CallExpr, arguments: Vec<Value>) -> RuntimeError {
        let frame = StackFrame {
            function: Rc::clone(&call.function),
            arguments: arguments.into_iter().map(debug_print_value).collect(),
            location: self.bottom_location,
        };
        self.call_stack.push(frame);
        self.bottom_location = Some(call.location);
        self
    }

    /// Finishes the unwinding process and dumps the error with the stack trace to stderr.
    pub fn print_backtrace(self, file_name: &str, source: &str, main: Rc<RefCell<Function>>) {
        // TODO(0.2): add colors and print the offending expression (possibly with codespan,
        // but it may look too similar to compilation errors, maybe use a different preset?).
        eprintln!("Runtime error: {}.", self.message);

        let source_lines = LineNumberMapper::new(source);
        let std_lines = LineNumberMapper::new(colang::stdlib::STD_SOURCE);

        let report_location = |location: Option<SourceOrigin>| match location {
            Some(location) => {
                let location = location.as_plain();
                match location.file {
                    InputSpanFile::UserProgram => {
                        format!("{}:{}", file_name, source_lines.line_number(location.start))
                    }
                    InputSpanFile::Std => {
                        format!("<std.co>:{}", std_lines.line_number(location.start))
                    }
                }
            }
            None => "<internal>".to_string(),
        };

        let bottom_frame = StackFrame {
            function: main,
            arguments: vec![],
            location: self.bottom_location,
        };

        let mut frames = self.call_stack;
        frames.push(bottom_frame);

        let frames = frames.into_iter().enumerate().map(|(index, frame)| {
            format!(
                "#{}: {}({}) at {}",
                index,
                frame.function.borrow().name,
                frame.arguments.join(", "),
                report_location(frame.location)
            )
        });

        eprintln!("\nCall stack (most nested call first):");
        for frame in frames {
            eprintln!("{}", frame);
        }
    }
}

/// A frame in an error stack trace.
pub struct StackFrame {
    /// The called function.
    function: Rc<RefCell<Function>>,

    /// The function arguments values at the time of call, stringified with debug printer.
    arguments: Vec<String>,

    /// The location reached _inside_ the called function.
    ///
    /// For frames corresponding to internal functions, this is `None`.
    location: Option<SourceOrigin>,
}

// TODO(0.2): add better debug printers.
fn debug_print_value(value: Value) -> String {
    let value = value.into_rvalue();
    match value {
        Rvalue::Int(x) => format!("{}", x),
        Rvalue::Bool(b) => String::from(if b { "true" } else { "false" }),
        Rvalue::Char(c) => format!("{}", c as char),
        Rvalue::Array(_) => "<array>".to_string(),
        Rvalue::Pointer(_) => "<pointer>".to_string(),
        Rvalue::Struct(_) => "<struct>".to_string(),
        Rvalue::Void => panic!("void value encountered"),
    }
}

/// A helper for mapping byte offsets to line numbers.
struct LineNumberMapper {
    line_starts: Vec<usize>,
}

impl LineNumberMapper {
    fn new(source: &str) -> LineNumberMapper {
        let line_starts = source
            .as_bytes()
            .iter()
            .enumerate()
            .filter(|(_, c)| **c == b'\n')
            .map(|(i, _)| i)
            .collect();
        LineNumberMapper { line_starts }
    }

    fn line_number(&self, byte_offset: usize) -> usize {
        match self.line_starts.binary_search(&byte_offset) {
            Ok(index) => index + 1,
            Err(index) => index + 1,
        }
    }
}
