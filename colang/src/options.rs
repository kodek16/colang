//! Options for controlling compiler behavior.

#[derive(Clone, Copy)]
pub enum ParserOptions {
    Old,
    Experimental,
}

impl ParserOptions {
    pub fn default() -> ParserOptions {
        ParserOptions::Old
    }
}

#[derive(Clone, Copy)]
pub struct AnalyzerOptions {
    pub no_std: bool,
}

impl AnalyzerOptions {
    pub fn default() -> AnalyzerOptions {
        AnalyzerOptions { no_std: false }
    }
}
