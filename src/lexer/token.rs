#[derive(Copy, Debug, PartialEq, Eq, Hash, Clone)]
pub enum TokenType {
    // tokens
    Illegal,
    Eof,
    Comma,
    Semicolon,
    Colon,

    Ident,
    Integer,
    Character,
    String,

    // 演算子
    Assign,
    PlusAssign,
    MinusAssign,
    AsteriskAssign,
    SlashAssign,
    PercentAssign,
    Bang,
    Plus,
    Minus,
    Increment,
    Decrement,
    Asterisk,
    Slash,
    Percent,
    Ampersand,
    Pipe,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Eq,
    NotEq,
    And,
    Or,
    Dot,
    Arrow,

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
    For,
    While,
    Do,
    Break,
    Continue,
    Return,
    Struct,
    TypeDef,
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
        "while" => TokenType::While,
        "do" => TokenType::Do,
        "for" => TokenType::For,
        "return" => TokenType::Return,
        "break" => TokenType::Break,
        "continue" => TokenType::Continue,
        "struct" => TokenType::Struct,
        "typedef" => TokenType::TypeDef,
        _ => TokenType::Ident,
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub raw_string: String,
    pub row: usize,
    pub col: usize,
}

impl Token {
    pub fn new() -> Token {
        Token {
            token_type: TokenType::Eof,
            raw_string: "".to_string(),
            row: 0,
            col: 0,
        }
    }

    pub fn literal(&self) -> String {
        match self.token_type {
            TokenType::Character => self.raw_string.trim_matches('\'').parse().unwrap(),
            TokenType::String => self.raw_string.trim_matches('"').parse().unwrap(),
            _ => self.raw_string.clone(),
        }
    }
}
