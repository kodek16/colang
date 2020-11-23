//! Implements an interactive terminal GUI for exploring ASTs.

use colang::ast::*;
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
                if let Some(span) = span {
                    styled.append_plain(&source_code[..span.start]);
                    styled.append_styled(
                        &source_code[span.start..span.end],
                        Style::from(Effect::Bold).combine(Effect::Reverse),
                    );
                    styled.append_plain(&source_code[span.end..]);
                } else {
                    styled.append_plain(source_code);
                }
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
    span: Option<InputSpan>,
}

impl Display for AstNodeContent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

// These implementations are very technical, and should mirror the type structure consistently.
// Refer to existing definitions and add new ones in the same style.
//
// It could be nice to have a #[derive(AstNode)] macro that would do this dirty job for us.

impl From<Program> for AstTree {
    fn from(program: Program) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<Program>"),
                span: None,
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
                        text: String::from("functions"),
                        span: None,
                    },
                    children: program.functions.into_iter().map(|x| x.into()).collect(),
                },
            ],
        }
    }
}

impl From<FunctionDef> for AstTree {
    fn from(function: FunctionDef) -> Self {
        let mut children = vec![field("name", function.name.into())];
        children.push(field_vec(
            "parameters",
            function.parameters.into_iter().map(|p| p.into()).collect(),
        ));
        if let Some(return_type) = function.return_type {
            children.push(field("return_type", return_type.into()));
        }
        // if let Some(body) = function.return_type {
        //     children.push(body.into());
        // }
        AstTree {
            node: AstNodeContent {
                text: String::from("<FunctionDef>"),
                span: Some(function.signature_span),
            },
            children,
        }
    }
}

impl From<Identifier> for AstTree {
    fn from(identifier: Identifier) -> Self {
        AstTree {
            node: AstNodeContent {
                text: format!("<Identifier>: {}", identifier.text),
                span: Some(identifier.span),
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
                text: String::from("<NormalParameter>"),
                span: Some(parameter.span),
            },
            children: vec![
                field("name", parameter.name.into()),
                field("type", parameter.type_.into()),
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
                span: Some(parameter.span),
            },
            children: vec![],
        }
    }
}

impl From<TypeExpr> for AstTree {
    fn from(type_expr: TypeExpr) -> Self {
        match type_expr {
            TypeExpr::Scalar(type_expr) => type_expr.into(),
            TypeExpr::Array(type_expr) => type_expr.into(),
            TypeExpr::Pointer(type_expr) => type_expr.into(),
            TypeExpr::TemplateInstance(type_expr) => type_expr.into(),
        }
    }
}

impl From<ScalarTypeExpr> for AstTree {
    fn from(type_expr: ScalarTypeExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<ScalarTypeExpr>"),
                span: Some(type_expr.span),
            },
            children: vec![field("name", type_expr.name.into())],
        }
    }
}

impl From<ArrayTypeExpr> for AstTree {
    fn from(type_expr: ArrayTypeExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<ArrayTypeExpr>"),
                span: Some(type_expr.span),
            },
            children: vec![field("element", (*type_expr.element).into())],
        }
    }
}

impl From<PointerTypeExpr> for AstTree {
    fn from(type_expr: PointerTypeExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<PointerTypeExpr>"),
                span: Some(type_expr.span),
            },
            children: vec![field("target", (*type_expr.target).into())],
        }
    }
}

impl From<TemplateInstanceTypeExpr> for AstTree {
    fn from(type_expr: TemplateInstanceTypeExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<TemplateInstanceTypeExpr>"),
                span: Some(type_expr.span),
            },
            children: vec![
                field("template", type_expr.template.into()),
                field_vec(
                    "type_arguments",
                    type_expr
                        .type_arguments
                        .into_iter()
                        .map(|a| a.into())
                        .collect(),
                ),
            ],
        }
    }
}

fn field(name: impl Into<String>, tree: AstTree) -> AstTree {
    AstTree {
        node: AstNodeContent {
            text: name.into(),
            span: tree.node.span,
        },
        children: vec![tree],
    }
}

fn field_vec(name: impl Into<String>, trees: Vec<AstTree>) -> AstTree {
    let span = trees
        .iter()
        .fold(None, |acc, tree| match (acc, tree.node.span) {
            (Some(acc), Some(span)) => Some(acc + span),
            (Some(acc), None) => Some(acc),
            (None, Some(span)) => Some(span),
            (None, None) => None,
        });
    AstTree {
        node: AstNodeContent {
            text: name.into(),
            span,
        },
        children: trees,
    }
}
