
pub struct Program {
    pub external_items: Vec<ExternalItem>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeRef {
    Named(String), // 名前付き型（例: int, char, void など）
    Pointer(Box<TypeRef>), // ポインタ型（例: int*）
    Array(Box<TypeRef>, Option<u32>), // 配列型（例: int[10]）
}

impl TypeRef {
    pub fn type_name(&self) -> String {
        match self {
            TypeRef::Named(name) => name.clone(),
            TypeRef::Pointer(type_ref) => type_ref.type_name() + "*",
            TypeRef::Array(type_ref, size) => format!("{}[{}]", type_ref.type_name(), size.map_or("".to_string(), |x| x.to_string())),
        }
    }
}

/// トップレベルの宣言・定義を表すノード
#[derive(Debug, PartialEq, Eq)]
pub enum ExternalItem {
    FunctionDecl { return_type_dec: TypeRef, name: String, parameters: Vec<Parameter>, body: Option<Box<Statement>> },
    VarDecl { declarators: Vec<(TypeRef, Declarator)> },
}

#[derive(Debug, PartialEq, Eq)]
pub struct Parameter {
    pub type_dec: TypeRef,
    pub name: String,
}

/// 文を表すノード
#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Return { value: Option<Expression> },
    Break,
    Continue,
    VarDecl { declarators: Vec<(TypeRef, Declarator)> },
    Block { statements: Vec<Statement> },
    If { condition: Expression, consequence: Box<Statement>, alternative: Option<Box<Statement>> },
    Switch { condition: Expression, switch_block: SwitchBlock },
    While { condition: Expression, body: Box<Statement> },
    DoWhile { body: Box<Statement>, condition: Expression },
    For { init: Option<Expression>, condition: Option<Expression>, post: Option<Expression>, body: Box<Statement> },
    ExpressionStatement { expression: Expression },
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
    Int { value: i32 },
    Identifier { value: String },
    PrefixExpression { operator: String, right: Box<Expression> },
    InfixExpression { operator: String, left: Box<Expression>, right: Box<Expression> },
    PostfixExpression { operator: String, left: Box<Expression> },
    FunctionCallExpression { function_name: String, arguments: Vec<Expression> },
    ArrayInitializerExpression { elements: Vec<Expression> },
    IndexExpression { left: Box<Expression>, index: Box<Expression> },
}

impl Statement {
    pub fn to_string(&self) -> String {
        return match self {
            Statement::Return { value } => {
                match value {
                    Some(v) => format!("return {};", v.to_string()),
                    None => "return;".to_string(),
                }
            },
            Statement::Break => "break;".to_string(),
            Statement::Continue => "continue;".to_string(),
            Statement::VarDecl { declarators } => {
                let mut parts: Vec<String> = vec![];
                for (type_ref, declarator) in declarators {
                    let mut s = format!("{} {}", type_ref.type_name(), declarator.name);
                    if let Some(value) = &declarator.value {
                        s.push_str(&format!(" = {}", value.to_string()));
                    }
                    parts.push(s);
                }
                format!("{};", parts.join(", "))
            },
            Statement::Block { statements } => {
                let mut result = "{\n".to_string();
                for stmt in statements {
                    result.push_str(&format!("    {}\n", stmt.to_string()));
                }
                result.push_str("}");
                result
            },
            Statement::If { condition, consequence, alternative } => {
                let mut result = format!("if ({}) {}", condition.to_string(), consequence.to_string());
                match alternative {
                    Some(alt) => {
                        result.push_str(&format!(" else {}", alt.to_string()));
                    },
                    None => {},
                }
                result
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
                result
            },
            Statement::While { condition, body } => {
                format!("while ({}) {}", condition.to_string(), body.to_string())
            },
            Statement::DoWhile { body, condition } => {
                format!("do {} while ({});", body.to_string(), condition.to_string())
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
                format!("for ({}, {}, {}) {}", init_str, condition_str, post_str, body.to_string())
            },
            Statement::ExpressionStatement { expression } => {
                format!("{};", expression.to_string())
            },
        };
    }
}

impl Expression {
    pub fn to_string(&self) -> String {
        return match self {
            Expression::Int { value } => value.to_string(),
            Expression::Identifier { value } => value.to_string(),
            Expression::PrefixExpression { operator, right } => {
                format!("({}{})", operator, right.to_string())
            },
            Expression::InfixExpression { operator, left, right } => {
                format!("({} {} {})", left.to_string(), operator, right.to_string())
            },
            Expression::PostfixExpression { operator, left } => {
                format!("({}{})", left.to_string(), operator)
            },
            Expression::FunctionCallExpression { function_name, arguments } => {
                let args: Vec<String> = arguments.iter().map(|arg| arg.to_string()).collect();
                format!("{}({})", function_name, args.join(", "))
            },
            Expression::ArrayInitializerExpression { elements } => {
                let args: Vec<String> = elements.iter().map(|arg| arg.to_string()).collect();
                format!("{{{}}}", args.join(", "))
            },
            Expression::IndexExpression { left, index } => {
                format!("({}[{}])", left.to_string(), index.to_string())
            },
        }
    }
}
