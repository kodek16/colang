use crate::parser::base::{Word, WordParser};

mod internal {
    pub struct KwFun;
    pub struct KwIf;
    pub struct KwVar;
}

impl Word for internal::KwFun {
    const WORD: &'static str = "fun";
}

impl Word for internal::KwIf {
    const WORD: &'static str = "if";
}

impl Word for internal::KwVar {
    const WORD: &'static str = "var";
}

pub type KwFun = WordParser<internal::KwFun>;
pub type KwIf = WordParser<internal::KwIf>;
pub type KwVar = WordParser<internal::KwVar>;
