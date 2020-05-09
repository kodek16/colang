//! A miniature s-expression library with pretty-printing support.

use std::cmp;

/// An s-expression in a slightly modified form with list support.
///
/// S-expressions are an extremely simple data encoding format. The variant used here represents
/// the encoded data as a tree where leaves are "atoms" and intermediate nodes are "lists".
#[derive(Debug)]
pub enum Sexp {
    /// A leaf in the s-expression tree.
    Atom(Atom),

    /// An intermediate node in the s-expression tree that contains other nodes.
    List(Vec<Sexp>),
}

/// A leaf in the s-expression tree.
///
/// The leaf payload can be either an integer or a string.
#[derive(Debug)]
pub enum Atom {
    Int(i32),
    String(String),
}

pub trait ToSexp {
    fn to_sexp(&self) -> Sexp;
}

impl Sexp {
    pub fn int(value: i32) -> Sexp {
        Sexp::Atom(Atom::Int(value))
    }

    pub fn str(value: impl Into<String>) -> Sexp {
        Sexp::Atom(Atom::String(value.into()))
    }

    /// Prints the expression with indents and line wrapping.
    pub fn pretty_print(&self, line_wrap_len: usize) -> String {
        match self {
            Sexp::List(elements) => {
                let elements: Vec<_> = elements
                    .iter()
                    .map(|element| element.pretty_print(cmp::max(line_wrap_len, 2) - 2))
                    .collect();

                let should_wrap = elements.iter().any(|element| element.contains('\n'))
                    || elements.iter().map(|elem| elem.len()).sum::<usize>() + elements.len() + 1
                        > line_wrap_len;

                if should_wrap {
                    if elements.is_empty() {
                        "()".to_string()
                    } else {
                        let elements: Vec<_> = elements.iter().map(|elem| indent(&elem)).collect();
                        let result: String = elements.join("\n");
                        format!("({})", &result[1..])
                    }
                } else {
                    format!("({})", elements.join(" "))
                }
            }
            Sexp::Atom(Atom::Int(value)) => value.to_string(),
            Sexp::Atom(Atom::String(value)) => value.clone(),
        }
    }
}

// With how the current Rust macro system works, this is unfortunately exported in the root crate,
// and so needs to be imported with `use crate::sexp_list`.
#[macro_export]
macro_rules! sexp_list {
    ( $( $elem:expr ),* ) => {{
        let mut temp_vec = Vec::new();
        $(
            temp_vec.push($elem);
        )*
        Sexp::List(temp_vec)
    }};
    ( $( $elem:expr ,) * ) => ( sexp_list ! [ $( $elem ) , * ] );
}

fn indent(text: &str) -> String {
    let lines: Vec<_> = text
        .lines()
        .map(|line| {
            if line.is_empty() {
                line.to_string()
            } else {
                format!(" {}", line)
            }
        })
        .collect();

    lines.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pretty_print_no_wrap() {
        let s = sexp_list!(Sexp::str("hello"), sexp_list!(Sexp::str("world")));
        assert_eq!(s.pretty_print(80), "(hello (world))")
    }

    #[test]
    fn test_pretty_print_wrap() {
        let s = sexp_list!(Sexp::str("hello"), sexp_list!(Sexp::str("world")));
        assert_eq!(s.pretty_print(8), "(hello\n (world))")
    }
}
