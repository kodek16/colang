use crate::parser::base::Word;

pub struct KwFun;

pub struct KwVar;

impl Word for KwFun {
    const WORD: &'static str = "fun";
}

impl Word for KwVar {
    const WORD: &'static str = "var";
}
