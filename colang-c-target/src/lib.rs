use crate::names::NumericCNameRegistry;
use crate::printer::CCodePrinter;
use colang::backends::Backend;
use colang::program::*;
use std::fs::File;
use std::io::Write;
use std::path::Path;

mod names;
mod prelude;
mod printer;

pub struct CBackend;

impl Backend for CBackend {
    fn run(&self, file_name: &str, _: &str, program: Program) -> Result<(), ()> {
        let mut names = NumericCNameRegistry::new();
        let mut printer = CCodePrinter::new();

        printer
            .write_program(&mut names, &program)
            .expect("Error occurred while writing the compiled C/C++ program");

        let target_path = Path::new(file_name).with_extension("cpp");
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
