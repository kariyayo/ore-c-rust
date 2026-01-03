use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TypeRef {
    // 名前付き型（例: int, char, void など）
    Named(String),
    // ポインタ型（例: int*）
    Pointer(Box<TypeRef>),
    // 配列型（例: int[10]）
    Array {
        type_ref: Box<TypeRef>,
        size: Option<u32>,
    },
    // 構造体
    Struct(StructRef),
    // typedef
    Typedef(Box<TypeRef>),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum StructRef {
    TagName(String),
    Decl(StructDecl),
}

impl TypeRef {
    pub fn type_name(&self) -> String {
        match self {
            TypeRef::Named(name) => name.clone(),
            TypeRef::Pointer(type_ref) => type_ref.type_name() + "*",
            TypeRef::Array { type_ref, size } => format!(
                "{}[{}]",
                type_ref.type_name(),
                size.map_or("".to_string(), |x| x.to_string())
            ),
            TypeRef::Struct(struct_ref) => match struct_ref {
                StructRef::TagName(tag_name) => {
                    format!("struct {}", tag_name)
                }
                StructRef::Decl(StructDecl { tag_name, members }) => {
                    if members.is_empty() {
                        format!(
                            "struct {}",
                            tag_name.as_ref().map_or("".to_string(), |x| x.to_string())
                        )
                    } else if tag_name.is_some() {
                        format!("struct {}", tag_name.as_ref().unwrap())
                    } else {
                        let members_string = members
                            .iter()
                            .map(|StructMember { type_ref, name }| {
                                type_ref.type_name() + " " + name
                            })
                            .fold("\n".to_string(), |acc, x| acc + &format!("    {};\n", x));
                        format!("struct {{{}}}", members_string)
                    }
                }
            },
            TypeRef::Typedef(type_ref) => {
                format!("typedef {}", type_ref.type_name())
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StructDecl {
    pub tag_name: TagName,
    pub members: Vec<StructMember>,
}

pub type TagName = Option<String>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StructMember {
    pub type_ref: TypeRef,
    pub name: String,
}

impl StructMember {
    pub fn new(p: (TypeRef, String)) -> StructMember {
        StructMember {
            type_ref: p.0,
            name: p.1,
        }
    }
}

impl fmt::Display for StructMember {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.type_ref.type_name(), self.name)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionDecl {
    pub return_type_ref: TypeRef,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Option<Box<StatementNode>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parameter {
    pub type_ref: TypeRef,
    pub name: String,
}

impl Parameter {
    pub fn new(p: (TypeRef, String)) -> Parameter {
        Parameter {
            type_ref: p.0,
            name: p.1,
        }
    }
}

/// ASTのルートノード
#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub external_item_nodes: Vec<ExternalItemNode>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Loc {
    pub row: usize,
    pub col: usize,
}

/// トップレベルの宣言・定義を表すノード
pub type ExternalItemNode = (ExternalItem, Loc);

#[derive(Debug, PartialEq, Eq, Clone)]
#[allow(clippy::enum_variant_names)]
pub enum ExternalItem {
    FunctionDeclNode(FunctionDecl),
    StructDeclNode(StructDecl),
    TypedefNode(TypeRef, Vec<String>),
    VarDeclNode(Vec<(TypeRef, Declarator)>),
}

/// 文を表すノード
pub type StatementNode = (Statement, Loc);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Statement {
    Return(Option<ExpressionNode>),
    Break,
    Continue,
    Typedef(TypeRef, Vec<String>),
    VarDecl(Vec<(TypeRef, Declarator)>),
    Block(Vec<StatementNode>),
    If {
        condition: ExpressionNode,
        consequence: Box<StatementNode>,
        alternative: Option<Box<StatementNode>>,
    },
    Switch {
        condition: ExpressionNode,
        switch_block: SwitchBlock,
    },
    While {
        condition: ExpressionNode,
        body: Box<StatementNode>,
    },
    DoWhile {
        body: Box<StatementNode>,
        condition: ExpressionNode,
    },
    For {
        init: Option<ExpressionNode>,
        condition: Option<ExpressionNode>,
        post: Option<ExpressionNode>,
        body: Box<StatementNode>,
    },
    #[allow(clippy::enum_variant_names)]
    ExpressionStatement(ExpressionNode),
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Declarator {
    pub name: String,
    pub value: Option<ExpressionNode>,
}

/// fallthrough に対応するため、switchブロック内の文を全てbodyにまとめておき、
/// labels でラベルと、文のまとまり内における開始位置を管理する
///
/// 例えば、以下のようなC言語コードの場合、
/// switch (x) {
/// case 1:
/// case 2:
///     printf("One or Two\n");
/// case 3:
///     printf("Three\n");
///     break;
/// }
///
/// この SwitchBody は以下のように表現される
/// SwitchBlock {
///     labels_entries: vec![
///         SwitchLabelEntry { labels: vec![Case(Int(1)), Case(Int(2))], start_index: 0 },
///         SwitchLabelEntry { labels: vec![Case(Int(3))], start_index: 1 },
///     ],
///     body: vec![
///         Statement(... "One or Two"),
///         Statement(... "Three"),
///         Break,
///     ],
/// }
///
#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct SwitchBlock {
    pub label_entries: Vec<SwitchLabelEntry>,
    pub body: Vec<StatementNode>,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct SwitchLabelEntry {
    pub labels: Vec<SwitchLabel>,
    pub start_index: i32,
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum SwitchLabel {
    Case(ExpressionNode),
    Default,
}

/// 式を表すノード
pub type ExpressionNode = (Expression, Loc);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum Expression {
    Int(i32),
    CharacterLiteral(char),
    StringLiteral(String),
    Identifier(String),
    Prefix {
        operator: String,
        right: Box<ExpressionNode>,
    },
    Infix {
        operator: String,
        left: Box<ExpressionNode>,
        right: Box<ExpressionNode>,
    },
    Postfix {
        operator: String,
        left: Box<ExpressionNode>,
    },
    FunctionCall {
        function_name: String,
        arguments: Vec<ExpressionNode>,
    },
    Initializer {
        elements: Vec<ExpressionNode>,
    },
    Index {
        left: Box<ExpressionNode>,
        index: Box<ExpressionNode>,
    },
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.external_item_nodes)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Return(value) => match value {
                Some((v, _)) => write!(f, "return {};", v),
                None => write!(f, "return;"),
            },
            Statement::Break => write!(f, "break;"),
            Statement::Continue => write!(f, "continue;"),
            Statement::Typedef(type_ref, items) => {
                write!(f, "{} {}", type_ref.type_name(), items.join(", "))
            }
            Statement::VarDecl(declarators) => {
                let mut parts: Vec<String> = vec![];
                for (type_ref, declarator) in declarators {
                    let mut s = format!("{} {}", type_ref.type_name(), declarator.name);
                    if let Some(value) = &declarator.value {
                        s.push_str(&format!(" = {}", value.0));
                    }
                    parts.push(s);
                }
                write!(f, "{};", parts.join(", "))
            }
            Statement::Block(statements) => {
                let mut result = "{\n".to_string();
                for (stmt, _) in statements {
                    result.push_str(&format!("    {}\n", stmt));
                }
                result.push('}');
                write!(f, "{}", result)
            }
            Statement::If {
                condition,
                consequence,
                alternative,
            } => {
                let mut result = format!("if ({}) {}", condition.0, consequence.0);
                if let Some(alt) = alternative {
                    result.push_str(&format!(" else {}", alt.0));
                }
                write!(f, "{}", result)
            }
            Statement::Switch {
                condition,
                switch_block: switch_body,
            } => {
                let mut result = format!("switch ({}) {{\n", condition.0);
                for label_entry in &switch_body.label_entries {
                    for label in &label_entry.labels {
                        match label {
                            SwitchLabel::Case(expr) => {
                                result.push_str(&format!("    case {}:\n", expr.0));
                            }
                            SwitchLabel::Default => {
                                result.push_str("    default:\n");
                            }
                        }
                    }
                    for stmt in &switch_body.body[label_entry.start_index as usize..] {
                        result.push_str(&format!("        {}\n", stmt.0));
                    }
                }
                result.push('}');
                write!(f, "{}", result)
            }
            Statement::While { condition, body } => {
                write!(f, "while ({}) {}", condition.0, body.0)
            }
            Statement::DoWhile { body, condition } => {
                write!(f, "do {} while ({});", body.0, condition.0)
            }
            Statement::For {
                init,
                condition,
                post,
                body,
            } => {
                let init_str = match init {
                    Some(i) => i.0.to_string(),
                    None => "".to_string(),
                };
                let condition_str = match condition {
                    Some(c) => c.0.to_string(),
                    None => "".to_string(),
                };
                let post_str = match post {
                    Some(p) => p.0.to_string(),
                    None => "".to_string(),
                };
                write!(
                    f,
                    "for ({}, {}, {}) {}",
                    init_str, condition_str, post_str, body.0
                )
            }
            Statement::ExpressionStatement(expression) => {
                write!(f, "{};", expression.0)
            }
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Int(value) => write!(f, "{}", value),
            Expression::CharacterLiteral(value) => write!(f, "{}", value),
            Expression::StringLiteral(value) => write!(f, "{}", value),
            Expression::Identifier(value) => write!(f, "{}", value),
            Expression::Prefix { operator, right } => {
                write!(f, "({}{})", operator, right.0)
            }
            Expression::Infix {
                operator,
                left,
                right,
            } => {
                write!(f, "({} {} {})", left.0, operator, right.0)
            }
            Expression::Postfix { operator, left } => {
                write!(f, "({}{})", left.0, operator)
            }
            Expression::FunctionCall {
                function_name,
                arguments,
            } => {
                let args: Vec<String> = arguments.iter().map(|arg| arg.0.to_string()).collect();
                write!(f, "{}({})", function_name, args.join(", "))
            }
            Expression::Initializer { elements } => {
                let args: Vec<String> = elements.iter().map(|arg| arg.0.to_string()).collect();
                write!(f, "{{{}}}", args.join(", "))
            }
            Expression::Index { left, index } => {
                write!(f, "({}[{}])", left.0, index.0)
            }
        }
    }
}
