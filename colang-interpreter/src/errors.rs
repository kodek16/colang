use crate::values::{Rvalue, Value};
use colang::program::{CallExpr, Function};
use colang::source::{InputSpanFile, SourceOrigin};
use std::cell::RefCell;
use std::rc::Rc;

pub type RunResult<T> = Result<T, RuntimeError>;

pub struct RuntimeError {
    message: String,

    /// Location not yet bound to a stack frame. The next added stack frame will be annotated
    /// with this location.
    bottom_location: Option<SourceOrigin>,
    call_stack: Vec<StackFrame>,
}

impl RuntimeError {
    pub fn new(message: impl Into<String>, location: Option<SourceOrigin>) -> RuntimeError {
        RuntimeError {
            message: message.into(),
            bottom_location: location,
            call_stack: Vec::new(),
        }
    }

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

pub struct StackFrame {
    function: Rc<RefCell<Function>>,
    arguments: Vec<String>,

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
