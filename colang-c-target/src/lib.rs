use crate::names::NumericCNameRegistry;
use crate::printer::CCodePrinter;
use colang::backends::Backend;
use colang::program::*;

mod names;
mod prelude;
mod printer;

pub struct CBackend;

impl Backend for CBackend {
    fn run(&self, _file_name: &str, _source: &str, program: Program) -> Result<(), ()> {
        let mut names = NumericCNameRegistry::new();
        let mut printer = CCodePrinter::new();

        printer
            .write_program(&mut names, &program)
            .expect("Error occurred while writing the compiled C/C++ program");
        println!("{}", printer);

        Ok(())
    }
}
