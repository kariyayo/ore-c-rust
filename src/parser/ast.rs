use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum TypeRef {
    Named(String), // 名前付き型（例: int, char, void など）
    Pointer(Box<TypeRef>), // ポインタ型（例: int*）
    Array{ type_dec: Box<TypeRef>, size: Option<u32>}, // 配列型（例: int[10]）
    Struct{ tag_name: Option<String>, members: Vec<StructDecl> }, // 構造体
}

impl TypeRef {
    pub fn type_name(&self) -> String {
        match self {
            TypeRef::Named(name) => name.clone(),
            TypeRef::Pointer(type_ref) => type_ref.type_name() + "*",
            TypeRef::Array { type_dec: type_ref, size } => format!("{}[{}]", type_ref.type_name(), size.map_or("".to_string(), |x| x.to_string())),
            TypeRef::Struct { tag_name, members } => {
                if members.len() == 0 {
                    format!("struct {}", tag_name.as_ref().map_or("".to_string(), |x| x.to_string()))
                } else if tag_name.is_some() {
                    format!("struct {}", tag_name.as_ref().unwrap())
                } else {
                    let members_string = members.iter()
                        .map(|StructDecl{ type_dec, name }| type_dec.type_name() + " " + name)
                        .fold("\n".to_string(), |acc, x| acc + &format!("    {};\n", x));
                    format!("struct {{{}}}", members_string)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct StructDecl {
    pub type_dec: TypeRef,
    pub name: String,
}

impl StructDecl {
    pub fn new(p: (TypeRef, String)) -> StructDecl {
        return StructDecl { type_dec: p.0, name: p.1 };
    }
}

impl fmt::Display for StructDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.type_dec.type_name(), self.name)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub return_type_dec: TypeRef,
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub body: Option<Box<StatementNode>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Parameter {
    pub type_dec: TypeRef,
    pub name: String,
}

impl Parameter {
    pub fn new(p: (TypeRef, String)) -> Parameter {
        return Parameter { type_dec: p.0, name: p.1 };
    }
}

/// ASTのルートノード
#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub external_item_nodes: Vec<ExternalItemNode>,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub struct Loc { pub row: usize, pub col: usize }

/// トップレベルの宣言・定義を表すノード
pub type ExternalItemNode = (ExternalItem, Loc);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExternalItem {
    FunctionDecl(Function),
    Struct(TypeRef),
    VarDecl(Vec<(TypeRef, Declarator)>),
}

/// 文を表すノード
pub type StatementNode = (Statement, Loc);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Statement {
    Return(Option<ExpressionNode>),
    Break,
    Continue,
    VarDecl(Vec<(TypeRef, Declarator)>),
    Block(Vec<StatementNode>),
    If { condition: ExpressionNode, consequence: Box<StatementNode>, alternative: Option<Box<StatementNode>> },
    Switch { condition: ExpressionNode, switch_block: SwitchBlock },
    While { condition: ExpressionNode, body: Box<StatementNode> },
    DoWhile { body: Box<StatementNode>, condition: ExpressionNode },
    For { init: Option<ExpressionNode>, condition: Option<ExpressionNode>, post: Option<ExpressionNode>, body: Box<StatementNode> },
    ExpressionStatement(ExpressionNode),
}

#[derive(Debug, PartialEq, Eq, Clone)]
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SwitchBlock {
    pub label_entries: Vec<SwitchLabelEntry>,
    pub body: Vec<StatementNode>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct SwitchLabelEntry {
    pub labels: Vec<SwitchLabel>,
    pub start_index: i32,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum SwitchLabel {
    Case(ExpressionNode),
    Default,
}

/// 式を表すノード
pub type ExpressionNode = (Expression, Loc);

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Expression {
    Int(i32),
    CharacterLiteral(char),
    StringLiteral(String),
    Identifier(String),
    PrefixExpression { operator: String, right: Box<ExpressionNode> },
    InfixExpression { operator: String, left: Box<ExpressionNode>, right: Box<ExpressionNode> },
    PostfixExpression { operator: String, left: Box<ExpressionNode> },
    FunctionCallExpression { function_name: String, arguments: Vec<ExpressionNode> },
    InitializerExpression { elements: Vec<ExpressionNode> },
    IndexExpression { left: Box<ExpressionNode>, index: Box<ExpressionNode> },
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.external_item_nodes)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            Statement::Return(value) => {
                match value {
                    Some((v, _)) => write!(f, "return {};", v.to_string()),
                    None => write!(f, "return;"),
                }
            },
            Statement::Break => write!(f, "break;"),
            Statement::Continue => write!(f, "continue;"),
            Statement::VarDecl(declarators) => {
                let mut parts: Vec<String> = vec![];
                for (type_ref, declarator) in declarators {
                    let mut s = format!("{} {}", type_ref.type_name(), declarator.name);
                    if let Some(value) = &declarator.value {
                        s.push_str(&format!(" = {}", value.0.to_string()));
                    }
                    parts.push(s);
                }
                write!(f, "{};", parts.join(", "))
            },
            Statement::Block(statements) => {
                let mut result = "{\n".to_string();
                for (stmt, _) in statements {
                    result.push_str(&format!("    {}\n", stmt.to_string()));
                }
                result.push_str("}");
                write!(f, "{}", result)
            },
            Statement::If { condition, consequence, alternative } => {
                let mut result = format!("if ({}) {}", condition.0.to_string(), consequence.0.to_string());
                match alternative {
                    Some(alt) => {
                        result.push_str(&format!(" else {}", alt.0.to_string()));
                    },
                    None => {},
                }
                write!(f, "{}", result)
            },
            Statement::Switch { condition, switch_block: switch_body } => {
                let mut result = format!("switch ({}) {{\n", condition.0.to_string());
                for label_entry in &switch_body.label_entries {
                    for label in &label_entry.labels {
                        match label {
                            SwitchLabel::Case(expr) => {
                                result.push_str(&format!("    case {}:\n", expr.0.to_string()));
                            },
                            SwitchLabel::Default => {
                                result.push_str("    default:\n");
                            },
                        }
                    }
                    for stmt in &switch_body.body[label_entry.start_index as usize..] {
                        result.push_str(&format!("        {}\n", stmt.0.to_string()));
                    }
                }
                result.push_str("}");
                write!(f, "{}", result)
            },
            Statement::While { condition, body } => {
                write!(f, "while ({}) {}", condition.0.to_string(), body.0.to_string())
            },
            Statement::DoWhile { body, condition } => {
                write!(f, "do {} while ({});", body.0.to_string(), condition.0.to_string())
            },
            Statement::For { init, condition, post, body } => {
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
                write!(f, "for ({}, {}, {}) {}", init_str, condition_str, post_str, body.0.to_string())
            },
            Statement::ExpressionStatement(expression) => {
                write!(f, "{};", expression.0.to_string())
            },
        };
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            Expression::Int(value) => write!(f, "{}",value),
            Expression::CharacterLiteral(value) => write!(f, "{}", value),
            Expression::StringLiteral(value) => write!(f, "{}", value),
            Expression::Identifier(value) => write!(f, "{}", value),
            Expression::PrefixExpression { operator, right } => {
                write!(f, "({}{})", operator, right.0.to_string())
            },
            Expression::InfixExpression { operator, left, right } => {
                write!(f, "({} {} {})", left.0.to_string(), operator, right.0.to_string())
            },
            Expression::PostfixExpression { operator, left } => {
                write!(f, "({}{})", left.0.to_string(), operator)
            },
            Expression::FunctionCallExpression { function_name, arguments } => {
                let args: Vec<String> = arguments.iter().map(|arg| arg.0.to_string()).collect();
                write!(f, "{}({})", function_name, args.join(", "))
            },
            Expression::InitializerExpression { elements } => {
                let args: Vec<String> = elements.iter().map(|arg| arg.0.to_string()).collect();
                write!(f, "{{{}}}", args.join(", "))
            },
            Expression::IndexExpression { left, index } => {
                write!(f, "({}[{}])", left.0.to_string(), index.0.to_string())
            },
        }
    }
}
