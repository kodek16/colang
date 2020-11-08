use crate::parser::base::{Word, WordParser};

mod internal {
    pub struct KwElse;
    pub struct KwFun;
    pub struct KwIf;
    pub struct KwVar;
}

impl Word for internal::KwElse {
    const WORD: &'static str = "else";
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

pub type KwElse = WordParser<internal::KwElse>;
pub type KwFun = WordParser<internal::KwFun>;
pub type KwIf = WordParser<internal::KwIf>;
pub type KwVar = WordParser<internal::KwVar>;
