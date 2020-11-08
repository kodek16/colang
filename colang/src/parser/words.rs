use crate::parser::base::{Word, WordParser};

mod internal {
    pub struct KwElse;
    pub struct KwFalse;
    pub struct KwFun;
    pub struct KwIf;
    pub struct KwTrue;
    pub struct KwVar;
}

impl Word for internal::KwElse {
    const WORD: &'static str = "else";
}

impl Word for internal::KwFalse {
    const WORD: &'static str = "false";
}

impl Word for internal::KwFun {
    const WORD: &'static str = "fun";
}

impl Word for internal::KwIf {
    const WORD: &'static str = "if";
}

impl Word for internal::KwTrue {
    const WORD: &'static str = "true";
}

impl Word for internal::KwVar {
    const WORD: &'static str = "var";
}

pub type KwElse = WordParser<internal::KwElse>;
pub type KwFalse = WordParser<internal::KwFalse>;
pub type KwFun = WordParser<internal::KwFun>;
pub type KwIf = WordParser<internal::KwIf>;
pub type KwTrue = WordParser<internal::KwTrue>;
pub type KwVar = WordParser<internal::KwVar>;
