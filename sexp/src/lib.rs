use std::cmp;

#[derive(Debug)]
pub enum Sexp {
    Atom(Atom),
    List(Vec<Sexp>),
}

#[derive(Debug)]
pub enum Atom {
    Int(i32),
    String(String),
}

pub trait ToSexp {
    fn to_sexp(&self) -> Sexp;
}

impl Sexp {
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

impl Atom {
    pub fn str(source: &str) -> Atom {
        Atom::String(source.to_string())
    }
}

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

#[macro_export]
macro_rules! sexp_str {
    ( $value:expr ) => {
        Sexp::Atom(Atom::String(($value).to_string()))
    };
}

#[macro_export]
macro_rules! sexp_string {
    ( $value:expr ) => {
        Sexp::Atom(Atom::String($value))
    };
}

#[macro_export]
macro_rules! sexp_int {
    ( $value:expr ) => {
        Sexp::Atom(Atom::Int($value))
    };
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
