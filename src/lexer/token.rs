#[derive(Copy, Debug, PartialEq, Eq, Hash, Clone)]
pub enum TokenType {
    // tokens
    Illegal,
    Eof,
    Comma,
    Semicolon,

    Ident,
    Integer,

    // 演算子
    Assign,
    Bang,
    Plus,
    Minus,
    Increment,
    Decrement,
    Asterisk,
    Slash,
    Lt,
    Gt,
    Eq,
    NotEq,

    // 括弧
    Lparem,
    Rparem,
    Lbrace,
    Rbrace,
    Lbracket,
    Rbracket,

    // キーワード
    Void,
    Char,
    Short,
    Int,
    Long,
    If,
    Else,
    Switch,
    Case,
    Default,
    Return,
}

impl TokenType {
    pub fn to_string(&self) -> &str {
        match self {
            TokenType::Illegal => "ILLEGAL",
            TokenType::Eof => "EOF",
            TokenType::Comma => ",",
            TokenType::Semicolon => ";",
            TokenType::Ident => "IDENT",
            TokenType::Integer => "INTEGER",
            TokenType::Assign => "=",
            TokenType::Bang => "!",
            TokenType::Plus => "+",
            TokenType::Minus => "-",
            TokenType::Increment => "++",
            TokenType::Decrement => "--",
            TokenType::Asterisk => "*",
            TokenType::Slash => "/",
            TokenType::Lt => "<",
            TokenType::Gt => ">",
            TokenType::Eq => "==",
            TokenType::NotEq => "!=",
            TokenType::Lparem => "(",
            TokenType::Rparem => ")",
            TokenType::Lbrace => "{",
            TokenType::Rbrace => "}",
            TokenType::Lbracket => "[",
            TokenType::Rbracket => "]",
            TokenType::Void => "VOID",
            TokenType::Char => "CHAR",
            TokenType::Short => "SHORT",
            TokenType::Int => "INT",
            TokenType::Long => "LONG",
            TokenType::If => "IF",
            TokenType::Else => "ELSE",
            TokenType::Switch => "SWITCH",
            TokenType::Case => "CASE",
            TokenType::Default => "DEFAULT",
            TokenType::Return => "RETURN",
        }
    }
}

pub fn lookup_ident(s: &str) -> TokenType {
    match s {
        "char" => TokenType::Char,
        "short" => TokenType::Short,
        "int" => TokenType::Int,
        "long" => TokenType::Long,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "switch" => TokenType::Switch,
        "case" => TokenType::Case,
        "default" => TokenType::Default,
        "return" => TokenType::Return,
        _ => TokenType::Ident,
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}
