use crate::names::NumericCNameRegistry;
use crate::printer::CCodePrinter;
use colang::backends::Backend;
use colang::program::*;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

mod names;
mod prelude;
mod printer;

pub struct CBackend {
    target_path: Option<PathBuf>,
}

impl CBackend {
    /// Initializes a C backend.
    ///
    /// `target_path` should be the desired path to the generated C file. If omitted, it is
    /// generated from source path by replacing `.co` extension with `.c`.
    pub fn new(target_path: Option<impl Into<PathBuf>>) -> CBackend {
        CBackend {
            target_path: target_path.map(|p| p.into()),
        }
    }
}

impl Backend for CBackend {
    fn run(&self, file_name: &str, source: &str, program: Program) -> Result<(), ()> {
        let mut names = NumericCNameRegistry::new();
        let mut printer = CCodePrinter::new();

        printer
            .write_program(&mut names, source, &program)
            .expect("Error occurred while writing the compiled C program");

        let target_path = self
            .target_path
            .clone()
            .unwrap_or_else(|| Path::new(file_name).with_extension("c"));
        let mut target_file = match File::create(&target_path) {
            Ok(file) => file,
            Err(error) => {
                eprintln!(
                    "Error: could not create output file \"{}\": {}",
                    target_path.display(),
                    error
                );
                return Err(());
            }
        };

        let target_code = format!("{}", printer);

        target_file
            .write_all(target_code.as_bytes())
            .map_err(|error| {
                eprintln!(
                    "Error: could not write resulting target file \"{}\": {}",
                    target_path.display(),
                    error
                );
            })
    }
}
