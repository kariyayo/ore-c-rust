
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TypeRef {
    pub type_name: String,
}

/// 文を表すノード
pub enum Statement {
    Return { value: Option<Expression> },
    VarDecl { type_dec: TypeRef, name: String, value: Option<Expression> },
    Block { statements: Vec<Statement> },
    If { condition: Expression, consequence: Box<Statement>, alternative: Option<Box<Statement>> },
    ExpressionStatement { expression: Expression },
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
