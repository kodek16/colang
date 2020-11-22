use colang::ast::{
    FunctionDef, Identifier, NormalParameter, Parameter, Program, SelfParameter, SelfParameterKind,
    TypeExpr,
};
use colang::source::InputSpan;
use cursive::theme::{BorderStyle, Color, Effect, Style};
use cursive::traits::*;
use cursive::utils::markup::StyledString;
use cursive::views::{LinearLayout, NamedView, Panel, ResizedView, TextView};
use cursive::Cursive;
use cursive_tree_view::{Placement, TreeView};
use std::fmt::{self, Display, Formatter};

pub fn display(source_code: &str, program: Program) {
    let mut siv = cursive::default();

    configure_theme(&mut siv);
    siv.add_global_callback('q', |s| s.quit());

    siv.add_fullscreen_layer(
        LinearLayout::horizontal()
            .child(ast_view(program))
            .child(source_code_view(source_code)),
    );

    siv.run();
}

fn configure_theme(siv: &mut Cursive) {
    let mut theme = siv.current_theme().clone();

    theme.shadow = false;
    theme.borders = BorderStyle::Simple;

    // A very basic dark theme.
    for key in &[
        "shadow",
        "view",
        "secondary",
        "tertiary",
        "title_primary",
        "title_secondary",
        "highlight",
        "highlight_inactive",
    ] {
        theme.palette.set_color(key, Color::TerminalDefault);
    }
    theme
        .palette
        .set_color("background", Color::parse("#262626").unwrap());
    theme
        .palette
        .set_color("primary", Color::parse("#bcbcbc").unwrap());

    siv.set_theme(theme);
}

fn ast_view(program: Program) -> ResizedView<Panel<NamedView<TreeView<AstNodeContent>>>> {
    let program: AstTree = program.into();
    let mut tree = TreeView::new();
    construct_tree_view(&mut tree, 0, program);

    tree.set_on_select(|siv: &mut Cursive, row: usize| {
        let span = siv
            .call_on_name("tree", move |tree: &mut TreeView<AstNodeContent>| {
                tree.borrow_item(row).unwrap().span
            })
            .unwrap();
        siv.call_on_name("source", move |text: &mut TextView| {
            let new_content = {
                let content = text.get_content();
                let source_code = content.source();
                let mut styled = StyledString::new();
                styled.append_plain(&source_code[..span.start]);
                styled.append_styled(
                    &source_code[span.start..span.end],
                    Style::from(Effect::Bold).combine(Effect::Reverse),
                );
                styled.append_plain(&source_code[span.end..]);
                styled
            };
            text.set_content(new_content);
        });
    });

    Panel::new(tree.with_name("tree")).full_width()
}

fn construct_tree_view(view: &mut TreeView<AstNodeContent>, row: usize, node: AstTree) {
    let row = view
        .insert_item(node.node, Placement::LastChild, row)
        .unwrap();
    for child in node.children {
        construct_tree_view(view, row, child);
    }
}

fn source_code_view(source_code: &str) -> ResizedView<Panel<LinearLayout>> {
    Panel::new(LinearLayout::vertical().child(TextView::new(source_code).with_name("source")))
        .full_width()
}

struct AstTree {
    node: AstNodeContent,
    children: Vec<AstTree>,
}

#[derive(Debug)]
struct AstNodeContent {
    text: String,
    span: InputSpan,
}

impl Display for AstNodeContent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

impl From<Program> for AstTree {
    fn from(program: Program) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("Program"),
                span: InputSpan::top_of_file(),
            },
            children: vec![
                // AstTree {
                //     node: String::from("Traits"),
                //     span: InputSpan::top_of_file(),
                //     children: program.traits.into_iter().map(|x| x.into()).collect(),
                // },
                // AstTree {
                //     node: String::from("Structs"),
                //     span: InputSpan::top_of_file(),
                //     children: program.structs.into_iter().map(|x| x.into()).collect(),
                // },
                AstTree {
                    node: AstNodeContent {
                        text: String::from("Functions"),
                        span: InputSpan::top_of_file(),
                    },
                    children: program.functions.into_iter().map(|x| x.into()).collect(),
                },
            ],
        }
    }
}

impl From<FunctionDef> for AstTree {
    fn from(function: FunctionDef) -> Self {
        let mut children = vec![wrap("Name", function.name.into())];
        for parameter in function.parameters {
            children.push(wrap("Function parameter", parameter.into()));
        }
        // if let Some(return_type) = function.return_type {
        //     children.push(return_type.into());
        // }
        // if let Some(body) = function.return_type {
        //     children.push(body.into());
        // }
        AstTree {
            node: AstNodeContent {
                text: String::from("FunctionDef"),
                span: function.signature_span,
            },
            children,
        }
    }
}

impl From<Identifier> for AstTree {
    fn from(identifier: Identifier) -> Self {
        AstTree {
            node: AstNodeContent {
                text: format!("Identifier: {}", identifier.text),
                span: identifier.span,
            },
            children: vec![],
        }
    }
}

impl From<Parameter> for AstTree {
    fn from(parameter: Parameter) -> Self {
        match parameter {
            Parameter::Normal(parameter) => parameter.into(),
            Parameter::Self_(parameter) => parameter.into(),
        }
    }
}

impl From<NormalParameter> for AstTree {
    fn from(parameter: NormalParameter) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("Parameter"),
                span: parameter.span,
            },
            children: vec![
                wrap("Name", parameter.name.into()),
                wrap("Type", parameter.type_.into()),
            ],
        }
    }
}

impl From<SelfParameter> for AstTree {
    fn from(parameter: SelfParameter) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from(match parameter.kind {
                    SelfParameterKind::ByValue => "self",
                    SelfParameterKind::ByPointer => "&self",
                }),
                span: parameter.span,
            },
            children: vec![],
        }
    }
}

impl From<TypeExpr> for AstTree {
    fn from(type_expr: TypeExpr) -> Self {
        // TODO dig into.
        AstTree {
            node: AstNodeContent {
                text: String::from("Type expression"),
                span: type_expr.span(),
            },
            children: vec![],
        }
    }
}

fn wrap(name: impl Into<String>, tree: AstTree) -> AstTree {
    AstTree {
        node: AstNodeContent {
            text: name.into(),
            span: tree.node.span,
        },
        children: vec![tree],
    }
}
