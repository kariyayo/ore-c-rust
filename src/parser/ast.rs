
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeRef {
    pub type_name: String,
}

/// 文を表すノード
#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Return { value: Option<Expression> },
    Break,
    Continue,
    VarDecl { type_dec: TypeRef, name: String, value: Option<Expression> },
    Block { statements: Vec<Statement> },
    If { condition: Expression, consequence: Box<Statement>, alternative: Option<Box<Statement>> },
    Switch { condition: Expression, switch_block: SwitchBlock },
    While { condition: Expression, body: Box<Statement> },
    DoWhile { body: Box<Statement>, condition: Expression },
    ExpressionStatement { expression: Expression },
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
            Statement::VarDecl { type_dec, name, value } => {
                match value {
                    Some(v) => format!("{} {} = {};", type_dec.type_name, name, v.to_string()),
                    None => format!("{} {};", type_dec.type_name, name),
                }
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
        }
    }
}
