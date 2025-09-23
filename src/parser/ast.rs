use std::fmt;

#[derive(Debug, PartialEq, Eq, Clone)]
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
                    format!( "struct {}", tag_name.as_ref().map_or("".to_string(), |x| x.to_string()))
                } else {
                    let members_string = members.iter()
                        .map(|StructDecl{ type_dec, name }| type_dec.type_name() + " " + name)
                        .fold("\n".to_string(), |acc, x| acc + &format!("    {};\n", x));
                    format!( "struct {}{{{}}}", tag_name.as_ref().map_or("".to_string(), |x| x.to_string() + " "), members_string)
                }
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
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

/// トップレベルの宣言・定義を表すノード
#[derive(Debug, PartialEq, Eq)]
pub enum ExternalItem {
    FunctionDecl { return_type_dec: TypeRef, name: String, parameters: Vec<Parameter>, body: Option<Box<Statement>> },
    Struct(TypeRef),
    VarDecl(Vec<(TypeRef, Declarator)>),
}

#[derive(Debug, PartialEq, Eq)]
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
    pub external_items: Vec<ExternalItem>,
}

/// 文を表すノード
#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Return(Option<Expression>),
    Break,
    Continue,
    VarDecl(Vec<(TypeRef, Declarator)>),
    Block(Vec<Statement>),
    If { condition: Expression, consequence: Box<Statement>, alternative: Option<Box<Statement>> },
    Switch { condition: Expression, switch_block: SwitchBlock },
    While { condition: Expression, body: Box<Statement> },
    DoWhile { body: Box<Statement>, condition: Expression },
    For { init: Option<Expression>, condition: Option<Expression>, post: Option<Expression>, body: Box<Statement> },
    ExpressionStatement(Expression),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Declarator {
    pub name: String,
    pub value: Option<Expression>,
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
#[derive(Debug, PartialEq, Eq)]
pub struct SwitchBlock {
    pub label_entries: Vec<SwitchLabelEntry>,
    pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SwitchLabelEntry {
    pub labels: Vec<SwitchLabel>,
    pub start_index: i32,
}

#[derive(Debug, PartialEq, Eq)]
pub enum SwitchLabel {
    Case(Expression),
    Default,
}

/// 式を表すノード
#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Int(i32),
    CharacterLiteral(char),
    StringLiteral(String),
    Identifier(String),
    PrefixExpression { operator: String, right: Box<Expression> },
    InfixExpression { operator: String, left: Box<Expression>, right: Box<Expression> },
    PostfixExpression { operator: String, left: Box<Expression> },
    FunctionCallExpression { function_name: String, arguments: Vec<Expression> },
    InitializerExpression { elements: Vec<Expression> },
    IndexExpression { left: Box<Expression>, index: Box<Expression> },
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.external_items)
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        return match self {
            Statement::Return(value) => {
                match value {
                    Some(v) => write!(f, "return {};", v.to_string()),
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
                        s.push_str(&format!(" = {}", value.to_string()));
                    }
                    parts.push(s);
                }
                write!(f, "{};", parts.join(", "))
            },
            Statement::Block(statements) => {
                let mut result = "{\n".to_string();
                for stmt in statements {
                    result.push_str(&format!("    {}\n", stmt.to_string()));
                }
                result.push_str("}");
                write!(f, "{}", result)
            },
            Statement::If { condition, consequence, alternative } => {
                let mut result = format!("if ({}) {}", condition.to_string(), consequence.to_string());
                match alternative {
                    Some(alt) => {
                        result.push_str(&format!(" else {}", alt.to_string()));
                    },
                    None => {},
                }
                write!(f, "{}", result)
            },
            Statement::Switch { condition, switch_block: switch_body } => {
                let mut result = format!("switch ({}) {{\n", condition.to_string());
                for label_entry in &switch_body.label_entries {
                    for label in &label_entry.labels {
                        match label {
                            SwitchLabel::Case(expr) => {
                                result.push_str(&format!("    case {}:\n", expr.to_string()));
                            },
                            SwitchLabel::Default => {
                                result.push_str("    default:\n");
                            },
                        }
                    }
                    for stmt in &switch_body.body[label_entry.start_index as usize..] {
                        result.push_str(&format!("        {}\n", stmt.to_string()));
                    }
                }
                result.push_str("}");
                write!(f, "{}", result)
            },
            Statement::While { condition, body } => {
                write!(f, "while ({}) {}", condition.to_string(), body.to_string())
            },
            Statement::DoWhile { body, condition } => {
                write!(f, "do {} while ({});", body.to_string(), condition.to_string())
            },
            Statement::For { init, condition, post, body } => {
                let init_str = match init {
                    Some(i) => i.to_string(),
                    None => "".to_string(),
                };
                let condition_str = match condition {
                    Some(c) => c.to_string(),
                    None => "".to_string(),
                };
                let post_str = match post {
                    Some(p) => p.to_string(),
                    None => "".to_string(),
                };
                write!(f, "for ({}, {}, {}) {}", init_str, condition_str, post_str, body.to_string())
            },
            Statement::ExpressionStatement(expression) => {
                write!(f, "{};", expression.to_string())
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
                write!(f, "({}{})", operator, right.to_string())
            },
            Expression::InfixExpression { operator, left, right } => {
                write!(f, "({} {} {})", left.to_string(), operator, right.to_string())
            },
            Expression::PostfixExpression { operator, left } => {
                write!(f, "({}{})", left.to_string(), operator)
            },
            Expression::FunctionCallExpression { function_name, arguments } => {
                let args: Vec<String> = arguments.iter().map(|arg| arg.to_string()).collect();
                write!(f, "{}({})", function_name, args.join(", "))
            },
            Expression::InitializerExpression { elements } => {
                let args: Vec<String> = elements.iter().map(|arg| arg.to_string()).collect();
                write!(f, "{{{}}}", args.join(", "))
            },
            Expression::IndexExpression { left, index } => {
                write!(f, "({}[{}])", left.to_string(), index.to_string())
            },
        }
    }
}
