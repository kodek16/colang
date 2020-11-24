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
    view.collapse_item(row);
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
                AstTree {
                    node: AstNodeContent {
                        text: String::from("structs"),
                        span: None,
                    },
                    children: program.structs.into_iter().map(|s| s.into()).collect(),
                },
                AstTree {
                    node: AstNodeContent {
                        text: String::from("traits"),
                        span: None,
                    },
                    children: program.traits.into_iter().map(|t| t.into()).collect(),
                },
                AstTree {
                    node: AstNodeContent {
                        text: String::from("functions"),
                        span: None,
                    },
                    children: program.functions.into_iter().map(|f| f.into()).collect(),
                },
            ],
        }
    }
}

impl From<TypeDef> for AstTree {
    fn from(type_def: TypeDef) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<TypeDef>"),
                span: Some(type_def.signature_span),
            },
            children: vec![
                field("name", type_def.name.into()),
                field_vec(
                    "type_parameters",
                    type_def
                        .type_parameters
                        .into_iter()
                        .map(|tp| tp.into())
                        .collect(),
                ),
                field_vec(
                    "implemented_traits",
                    type_def
                        .implemented_traits
                        .into_iter()
                        .map(|te| te.into())
                        .collect(),
                ),
                field_vec(
                    "fields",
                    type_def.fields.into_iter().map(|f| f.into()).collect(),
                ),
                field_vec(
                    "methods",
                    type_def.methods.into_iter().map(|m| m.into()).collect(),
                ),
            ],
        }
    }
}

impl From<TypeParameter> for AstTree {
    fn from(type_parameter: TypeParameter) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<TypeParameter>"),
                span: Some(type_parameter.span),
            },
            children: vec![
                field("name", type_parameter.name.into()),
                field_vec(
                    "trait_bounds",
                    type_parameter
                        .trait_bounds
                        .into_iter()
                        .map(|te| te.into())
                        .collect(),
                ),
            ],
        }
    }
}

impl From<FieldDef> for AstTree {
    fn from(field_def: FieldDef) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<FieldDef>"),
                span: Some(field_def.span),
            },
            children: vec![
                field("name", field_def.name.into()),
                field("type_", field_def.type_.into()),
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
        if let Some(body) = function.body {
            children.push(field("body", body.into()));
        }
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

impl From<ExpressionLike> for AstTree {
    fn from(expression: ExpressionLike) -> Self {
        use ExpressionLike::*;
        match expression {
            Variable(expression) => expression.into(),
            IntLiteral(expression) => expression.into(),
            BoolLiteral(expression) => expression.into(),
            CharLiteral(expression) => expression.into(),
            StringLiteral(expression) => expression.into(),
            Null(expression) => expression.into(),
            Self_(expression) => expression.into(),
            UnaryOp(expression) => expression.into(),
            BinaryOp(expression) => expression.into(),
            Address(expression) => expression.into(),
            Deref(expression) => expression.into(),
            New(expression) => expression.into(),
            Is(expression) => expression.into(),
            ArrayFromElements(expression) => expression.into(),
            ArrayFromCopy(expression) => expression.into(),
            Index(expression) => expression.into(),
            Call(expression) => expression.into(),
            FieldAccess(expression) => expression.into(),
            MethodCall(expression) => expression.into(),
            If(expression) => expression.into(),
            Block(expression) => expression.into(),
        }
    }
}

impl From<VariableExpr> for AstTree {
    fn from(expression: VariableExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<VariableExpr>"),
                span: Some(expression.span),
            },
            children: vec![field("name", expression.name.into())],
        }
    }
}

impl From<IntLiteralExpr> for AstTree {
    fn from(expression: IntLiteralExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: format!("<IntLiteralExpr>: {}", expression.value),
                span: Some(expression.span),
            },
            children: vec![],
        }
    }
}

impl From<BoolLiteralExpr> for AstTree {
    fn from(expression: BoolLiteralExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: format!("<BoolLiteralExpr>: {}", expression.value),
                span: Some(expression.span),
            },
            children: vec![],
        }
    }
}

impl From<CharLiteralExpr> for AstTree {
    fn from(expression: CharLiteralExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: format!("<CharLiteralExpr>: '{}'", expression.value),
                span: Some(expression.span),
            },
            children: vec![],
        }
    }
}

impl From<StringLiteralExpr> for AstTree {
    fn from(expression: StringLiteralExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: format!("<StringLiteralExpr>: \"{}\"", expression.value),
                span: Some(expression.span),
            },
            children: vec![],
        }
    }
}

impl From<NullExpr> for AstTree {
    fn from(expression: NullExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<NullExpr>"),
                span: Some(expression.span),
            },
            children: vec![],
        }
    }
}

impl From<SelfExpr> for AstTree {
    fn from(expression: SelfExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<SelfExpr>"),
                span: Some(expression.span),
            },
            children: vec![],
        }
    }
}

impl From<UnaryOperatorExpr> for AstTree {
    fn from(expression: UnaryOperatorExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: format!("<UnaryOperatorExpr>: {}", expression.operator),
                span: Some(expression.span),
            },
            children: vec![field("operand", (*expression.operand).into())],
        }
    }
}

impl From<BinaryOperatorExpr> for AstTree {
    fn from(expression: BinaryOperatorExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: format!("<BinaryOperatorExpr>: {}", expression.operator),
                span: Some(expression.span),
            },
            children: vec![
                field("lhs", (*expression.lhs).into()),
                field("rhs", (*expression.rhs).into()),
            ],
        }
    }
}

impl From<AddressExpr> for AstTree {
    fn from(expression: AddressExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<AddressExpr>"),
                span: Some(expression.span),
            },
            children: vec![field("target", (*expression.target).into())],
        }
    }
}

impl From<DerefExpr> for AstTree {
    fn from(expression: DerefExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<DerefExpr>"),
                span: Some(expression.span),
            },
            children: vec![field("pointer", (*expression.pointer).into())],
        }
    }
}

impl From<NewExpr> for AstTree {
    fn from(expression: NewExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<NewExpr>"),
                span: Some(expression.span),
            },
            children: vec![field("target_type", expression.target_type.into())],
        }
    }
}

impl From<IsExpr> for AstTree {
    fn from(expression: IsExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<IsExpr>"),
                span: Some(expression.span),
            },
            children: vec![
                field("lhs", (*expression.lhs).into()),
                field("rhs", (*expression.rhs).into()),
            ],
        }
    }
}

impl From<ArrayFromElementsExpr> for AstTree {
    fn from(expression: ArrayFromElementsExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<ArrayFromElementsExpr>"),
                span: Some(expression.span),
            },
            children: vec![field_vec(
                "elements",
                expression.elements.into_iter().map(|e| e.into()).collect(),
            )],
        }
    }
}

impl From<ArrayFromCopyExpr> for AstTree {
    fn from(expression: ArrayFromCopyExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<ArrayFromCopyExpr>"),
                span: Some(expression.span),
            },
            children: vec![
                field("element", (*expression.element).into()),
                field("size", (*expression.size).into()),
            ],
        }
    }
}

impl From<IndexExpr> for AstTree {
    fn from(expression: IndexExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<IndexExpr>"),
                span: Some(expression.span),
            },
            children: vec![
                field("collection", (*expression.collection).into()),
                field("index", (*expression.index).into()),
            ],
        }
    }
}

impl From<FieldAccessExpr> for AstTree {
    fn from(expression: FieldAccessExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<FieldAccessExpr>"),
                span: Some(expression.span),
            },
            children: vec![
                field("receiver", (*expression.receiver).into()),
                field("field", expression.field.into()),
            ],
        }
    }
}

impl From<CallExpr> for AstTree {
    fn from(expression: CallExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<CallExpr>"),
                span: Some(expression.span),
            },
            children: vec![
                field("function_name", expression.function_name.into()),
                field_vec(
                    "arguments",
                    expression.arguments.into_iter().map(|a| a.into()).collect(),
                ),
            ],
        }
    }
}

impl From<MethodCallExpr> for AstTree {
    fn from(expression: MethodCallExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<MethodCallExpr>"),
                span: Some(expression.span),
            },
            children: vec![
                field("receiver", (*expression.receiver).into()),
                field("method", expression.method.into()),
                field_vec(
                    "arguments",
                    expression.arguments.into_iter().map(|a| a.into()).collect(),
                ),
            ],
        }
    }
}

impl From<IfExpr> for AstTree {
    fn from(expression: IfExpr) -> Self {
        let mut children = vec![
            field("cond", (*expression.cond).into()),
            field("then", (*expression.then).into()),
        ];
        if let Some(else_) = expression.else_ {
            children.push((*else_).into());
        }
        AstTree {
            node: AstNodeContent {
                text: String::from("<IfExpr>"),
                span: Some(expression.span),
            },
            children,
        }
    }
}

impl From<BlockExpr> for AstTree {
    fn from(expression: BlockExpr) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<BlockExpr>"),
                span: Some(expression.span),
            },
            children: vec![field_vec(
                "items",
                expression.items.into_iter().map(|i| i.into()).collect(),
            )],
        }
    }
}

impl From<StmtOrExpr> for AstTree {
    fn from(stmt_or_expr: StmtOrExpr) -> Self {
        use StmtOrExpr::*;
        match stmt_or_expr {
            VarDecl(statement) => statement.into(),
            Read(statement) => statement.into(),
            Write(statement) => statement.into(),
            While(statement) => statement.into(),
            Assign(statement) => statement.into(),
            Return(statement) => statement.into(),
            Semicolon(statement) => statement.into(),
            ExprLike(expression) => expression.into(),
        }
    }
}

impl From<VarDeclStmt> for AstTree {
    fn from(statement: VarDeclStmt) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<VarDeclStmt>"),
                span: Some(statement.span),
            },
            children: vec![field_vec(
                "entries",
                statement.entries.into_iter().map(|e| e.into()).collect(),
            )],
        }
    }
}

impl From<VarDeclEntry> for AstTree {
    fn from(entry: VarDeclEntry) -> Self {
        let mut children = vec![field("variable_name", entry.variable_name.into())];
        if let Some(variable_type) = entry.variable_type {
            children.push(field("variable_type", variable_type.into()));
        }
        if let Some(initializer) = entry.initializer {
            children.push(field("initializer", initializer.into()));
        }
        AstTree {
            node: AstNodeContent {
                text: String::from("<VarDeclEntry>"),
                span: Some(entry.span),
            },
            children,
        }
    }
}

impl From<ReadStmt> for AstTree {
    fn from(statement: ReadStmt) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from(if statement.whole_line {
                    "<ReadStmt>: whole_line"
                } else {
                    "<ReadStmt>"
                }),
                span: Some(statement.span),
            },
            children: vec![field_vec(
                "entries",
                statement.entries.into_iter().map(|e| e.into()).collect(),
            )],
        }
    }
}

impl From<ReadEntry> for AstTree {
    fn from(entry: ReadEntry) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<ReadEntry>"),
                span: Some(entry.span),
            },
            children: vec![field("target", entry.target.into())],
        }
    }
}

impl From<WriteStmt> for AstTree {
    fn from(statement: WriteStmt) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from(if statement.newline {
                    "<WriteStmt>: newline"
                } else {
                    "<WriteStmt>"
                }),
                span: Some(statement.span),
            },
            children: vec![field("expression", statement.expression.into())],
        }
    }
}

impl From<WhileStmt> for AstTree {
    fn from(statement: WhileStmt) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<WhileStmt>"),
                span: Some(statement.span),
            },
            children: vec![
                field("cond", (*statement.cond).into()),
                field("body", (*statement.body).into()),
            ],
        }
    }
}

impl From<AssignStmt> for AstTree {
    fn from(statement: AssignStmt) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<AssignStmt>"),
                span: Some(statement.span),
            },
            children: vec![
                field("lhs", (*statement.lhs).into()),
                field("rhs", (*statement.rhs).into()),
            ],
        }
    }
}

impl From<ReturnStmt> for AstTree {
    fn from(statement: ReturnStmt) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<ReturnStmt>"),
                span: Some(statement.span),
            },
            children: statement.expression.into_iter().map(|e| e.into()).collect(),
        }
    }
}

impl From<SemicolonStmt> for AstTree {
    fn from(statement: SemicolonStmt) -> Self {
        AstTree {
            node: AstNodeContent {
                text: String::from("<SemicolonStmt>"),
                span: Some(statement.span),
            },
            children: vec![],
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
