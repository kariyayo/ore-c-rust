
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
