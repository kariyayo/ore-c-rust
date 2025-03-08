pub type TokenType = &'static str;

// tokens
pub const ILLEGAL: TokenType = "ILLEGAL";
pub const EOF: TokenType = "EOF";

pub const COMMA: TokenType = ",";
pub const SEMICOLON: TokenType = ";";

pub const IDENT: TokenType = "IDENT";
pub const INTEGER: TokenType = "INTEGER";

// 演算子
pub const ASSIGN: TokenType = "=";
pub const BANG: TokenType = "!";
pub const PLUS: TokenType = "+";
pub const MINUS: TokenType = "-";
pub const ASTERISK: TokenType = "*";
pub const SLASH: TokenType = "/";
pub const LT: TokenType = "<";
pub const RT: TokenType = ">";
pub const EQ: TokenType = "==";
pub const NOT_EQ: TokenType = "!=";

// 括弧
pub const LPAREM: TokenType = "(";
pub const RPAREM: TokenType = ")";
pub const LBRACE: TokenType = "{";
pub const RBRACE: TokenType = "}";
pub const LBRACKET: TokenType = "[";
pub const RBRACKET: TokenType = "]";

// キーワード
pub const VOID: TokenType = "VOID";
pub const CHAR: TokenType = "CHAR";
pub const SHORT: TokenType = "SHORT";
pub const INT: TokenType = "INT";
pub const LONG: TokenType = "LONG";
pub const IF: TokenType = "IF";
pub const ELSE: TokenType = "ELSE";
pub const SWITCH: TokenType = "SWITCH";
pub const CASE: TokenType = "CASE";
pub const DEFAULT: TokenType = "DEFAULT";
pub const RETURN: TokenType = "RETURN";

pub fn lookup_ident(s: &str) -> TokenType {
    match s {
        "char" => CHAR,
        "short" => SHORT,
        "int" => INT,
        "long" => LONG,
        "if" => IF,
        "else" => ELSE,
        "switch" => SWITCH,
        "case" => CASE,
        "default" => DEFAULT,
        "return" => RETURN,
        _ => IDENT,
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}
