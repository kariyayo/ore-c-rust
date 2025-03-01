// tokens
pub const ILLEGAL: &str = "ILLEGAL";
pub const EOF: &str     = "EOF";

pub const COMMA: &str     = ",";
pub const SEMICOLON: &str = ";";

pub const IDENT: &str   = "IDENT";
pub const INTEGER: &str = "INTEGER";

// 演算子
pub const ASSIGN: &str   = "=";
pub const BANG: &str     = "!";
pub const PLUS: &str     = "+";
pub const MINUS: &str    = "-";
pub const ASTERISK: &str = "*";
pub const SLASH: &str    = "/";
pub const LT: &str       = "<";
pub const RT: &str       = ">";
pub const EQ: &str       = "==";
pub const NOT_EQ: &str   = "!=";

// 括弧
pub const LPAREM: &str   = "(";
pub const RPAREM: &str   = ")";
pub const LBRACE: &str   = "{";
pub const RBRACE: &str   = "}";
pub const LBRACKET: &str = "[";
pub const RBRACKET: &str = "]";

// キーワード
pub const VOID: &str     = "VOID";
pub const CHAR: &str     = "CHAR";
pub const SHORT: &str    = "SHORT";
pub const INT: &str      = "INT";
pub const LONG: &str     = "LONG";
pub const IF: &str       = "IF";
pub const ELSE: &str     = "ELSE";
pub const SWITCH: &str   = "SWITCH";
pub const CASE: &str     = "CASE";
pub const DEFAULT: &str  = "DEFAULT";
pub const RETURN: &str   = "RETURN";

pub fn lookup_ident(s: &str) -> String {
  match s {
    "char" => CHAR.to_string(),
    "short" => SHORT.to_string(),
    "int" => INT.to_string(),
    "long" => LONG.to_string(),
    "if" => IF.to_string(),
    "else" => ELSE.to_string(),
    "switch" => SWITCH.to_string(),
    "case" => CASE.to_string(),
    "default" => DEFAULT.to_string(),
    "return" => RETURN.to_string(),
    _ => IDENT.to_string(),
  }
}

#[derive(Debug)]
pub struct Token {
  pub token_type: String,
  pub literal: String,
}
