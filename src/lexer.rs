use token::{Token, TokenType};

pub(crate) mod token;

pub(crate) struct Lexer {
    input: String,
    position: usize,      // 現在の位置
    read_position: usize, // これから読み込む位置（現在の文字の次）
    ch: char,             // 何も読んでない or ファイルの終わりを示す場合はNULL文字になる
    row: usize,
    col: usize,
}

impl Lexer {
    pub(crate) fn new(input: &str) -> Lexer {
        let mut l = Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: '\u{0}',
            row: 1,
            col: 0,
        };
        l.read_char();
        return l;
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\u{0}';
        } else {
            self.ch = self
                .input
                .chars()
                .nth(self.read_position)
                .unwrap_or('\u{0}');
        }
        self.position = self.read_position;
        self.read_position += 1;
        self.col += 1;
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            if self.ch == '\n' || self.ch == '\r' {
                self.row += 1;
                self.col = 0;
            }
            self.read_char();
        }
    }

    fn is_letter(&self, ch: char) -> bool {
        return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';
    }

    fn read_identifier(&mut self) -> String {
        let start_pos = self.position;
        while self.is_letter(self.ch) || self.is_digit(self.ch) {
            self.read_char();
        }
        return self.input[start_pos..self.position].to_string();
    }

    fn is_digit(&self, ch: char) -> bool {
        return '0' <= ch && ch <= '9';
    }

    fn read_number(&mut self) -> String {
        let start_pos = self.position;
        while self.is_digit(self.ch) {
            self.read_char();
        }
        return self.input[start_pos..self.position].to_string();
    }

    fn peek_char(&self) -> char {
        return self
            .input
            .chars()
            .nth(self.read_position)
            .unwrap_or('\u{0}');
    }

    fn read_character_raw(&mut self) -> String {
        self.read_char();
        return if self.ch == '\\' {
            self.read_char();
            let c = self.ch;
            self.read_char();
            format!("'\\{}'", c)
        } else {
            let c = self.ch;
            self.read_char();
            format!("'{}'", c)
        };
    }

    fn read_string_raw(&mut self) -> String {
        self.read_char();
        let start_pos = self.position;
        let mut before = '"';
        while self.ch != '"' || before == '\\' {
            before = self.ch;
            self.read_char();
        }
        let s = self.input[start_pos..self.position].to_string();
        format!("\"{}\"", s)
    }

    pub(crate) fn next_token(&mut self) -> token::Token {
        self.skip_whitespace();
        if self.ch == '#' {
            // TODO
            self.read_char();
            while self.ch != '\r' && self.ch != '\n' {
                self.read_char();
            }
            self.skip_whitespace();
        }
        if self.ch == '/' && self.peek_char() == '/' {
            self.read_char();
            self.read_char();
            while self.ch != '\r' && self.ch != '\n' {
                self.read_char();
            }
            self.skip_whitespace();
        }
        let (token_type, raw, skip_read) = match self.ch {
            ',' => (TokenType::Comma, self.ch.to_string(), false),
            ';' => (TokenType::Semicolon, self.ch.to_string(), false),
            ':' => (TokenType::Colon, self.ch.to_string(), false),
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    (TokenType::Eq, "==".to_string(), false)
                } else {
                    (TokenType::Assign, self.ch.to_string(), false)
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    (TokenType::NotEq, "!=".to_string(), false)
                } else {
                    (TokenType::Bang, self.ch.to_string(), false)
                }
            }
            '+' => {
                if self.peek_char() == '+' {
                    self.read_char();
                    (TokenType::Increment, "++".to_string(), false)
                } else if self.peek_char() == '=' {
                    self.read_char();
                    (TokenType::PlusAssign, "+=".to_string(), false)
                } else {
                    (TokenType::Plus, self.ch.to_string(), false)
                }
            },
            '-' => {
                if self.peek_char() == '-' {
                    self.read_char();
                    (TokenType::Decrement, "--".to_string(), false)
                } else if self.peek_char() == '=' {
                    self.read_char();
                    (TokenType::MinusAssign, "-=".to_string(), false)
                } else if self.peek_char() == '>' {
                    self.read_char();
                    (TokenType::Arrow, "->".to_string(), false)
                } else {
                    (TokenType::Minus, self.ch.to_string(), false)
                }
            },
            '*' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    (TokenType::AsteriskAssign, "*=".to_string(), false)
                } else {
                    (TokenType::Asterisk, self.ch.to_string(), false)
                }
            },
            '/' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    (TokenType::SlashAssign, "/=".to_string(), false)
                } else {
                    (TokenType::Slash, self.ch.to_string(), false)
                }
            },
            '%' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    (TokenType::PercentAssign, "%=".to_string(), false)
                } else {
                    (TokenType::Percent, self.ch.to_string(), false)
                }
            },
            '&' => (TokenType::Ampersand, self.ch.to_string(), false),
            '<' => (TokenType::Lt, self.ch.to_string(), false),
            '>' => (TokenType::Gt, self.ch.to_string(), false),
            '(' => (TokenType::Lparem, self.ch.to_string(), false),
            ')' => (TokenType::Rparem, self.ch.to_string(), false),
            '{' => (TokenType::Lbrace, self.ch.to_string(), false),
            '}' => (TokenType::Rbrace, self.ch.to_string(), false),
            '[' => (TokenType::Lbracket, self.ch.to_string(), false),
            ']' => (TokenType::Rbracket, self.ch.to_string(), false),
            '.' => (TokenType::Dot, self.ch.to_string(), false),
            '\u{0}' => (TokenType::Eof, "".to_string(), false),
            '\'' => (TokenType::Character, self.read_character_raw(), false),
            '"' => (TokenType::String, self.read_string_raw(), false),
            _ => {
                if self.is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token_type = token::lookup_ident(literal.as_str());
                    (token_type, literal, true)
                } else if self.is_digit(self.ch) {
                    let literal = self.read_number();
                    (TokenType::Integer, literal, true)
                } else {
                    (TokenType::Illegal, self.ch.to_string(), false)
                }
            }
        };
        if !skip_read {
            self.read_char();
        }
        let raw_len = raw.len();
        return Token {
            token_type,
            raw_string: raw,
            row: self.row,
            col: self.col - raw_len,
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = 
"#include <stdio.h>
int five = 5;

int add(int x, int y) {
    return x + y;
}

// comment

int main(int argc, char *argv[]) {
    int ten = 10;
    int result = add(five, ten);
}

10 == 10;
10 != 9;

++a1

!-/*%&5

x = 10
a += 5
a -= 5
a *= 5
a /= 5
a %= 5

if (5 < 10) {
    return 1;
} else {
    return 0;
}
switch (x) {
    case 1:
        return 1;
    default:
        return 0;
}

while (x < 10) continue;

do {
    return 1;
} while (x < 10);

for (;;) ++a;
[1, 2]
struct a {};
point.x;
p->x;
'A';
'\\'';
'\\n';
'\\\\';
\"Hello, World!\";
\" escape \\\" \\n \";
";
        let tests = vec![
            // (TokenType, "literal", row, col)
            (TokenType::Int, "int", 2, 1),
            (TokenType::Ident, "five", 2, 5),
            (TokenType::Assign, "=", 2, 10),
            (TokenType::Integer, "5", 2, 12),
            (TokenType::Semicolon, ";", 2, 13),
            (TokenType::Int, "int", 4, 1),
            (TokenType::Ident, "add", 4, 5),
            (TokenType::Lparem, "(", 4, 8),
            (TokenType::Int, "int", 4, 9),
            (TokenType::Ident, "x", 4, 13),
            (TokenType::Comma, ",", 4, 14),
            (TokenType::Int, "int", 4, 16),
            (TokenType::Ident, "y", 4, 20),
            (TokenType::Rparem, ")", 4, 21),
            (TokenType::Lbrace, "{", 4, 23),
            (TokenType::Return, "return", 5, 5),
            (TokenType::Ident, "x", 5, 12),
            (TokenType::Plus, "+", 5, 14),
            (TokenType::Ident, "y", 5, 16),
            (TokenType::Semicolon, ";", 5, 17),
            (TokenType::Rbrace, "}", 6, 1),
            (TokenType::Int, "int", 10, 1),
            (TokenType::Ident, "main", 10, 5),
            (TokenType::Lparem, "(", 10, 9),
            (TokenType::Int, "int", 10, 10),
            (TokenType::Ident, "argc", 10, 14),
            (TokenType::Comma, ",", 10, 18),
            (TokenType::Char, "char", 10, 20),
            (TokenType::Asterisk, "*", 10, 25),
            (TokenType::Ident, "argv", 10, 26),
            (TokenType::Lbracket, "[", 10, 30),
            (TokenType::Rbracket, "]", 10, 31),
            (TokenType::Rparem, ")", 10, 32),
            (TokenType::Lbrace, "{", 10, 34),
            (TokenType::Int, "int", 11, 5),
            (TokenType::Ident, "ten", 11, 9),
            (TokenType::Assign, "=", 11, 13),
            (TokenType::Integer, "10", 11, 15),
            (TokenType::Semicolon, ";", 11, 17),
            (TokenType::Int, "int", 12, 5),
            (TokenType::Ident, "result", 12, 9),
            (TokenType::Assign, "=", 12, 16),
            (TokenType::Ident, "add", 12, 18),
            (TokenType::Lparem, "(", 12, 21),
            (TokenType::Ident, "five", 12, 22),
            (TokenType::Comma, ",", 12, 26),
            (TokenType::Ident, "ten", 12, 28),
            (TokenType::Rparem, ")", 12, 31),
            (TokenType::Semicolon, ";", 12, 32),
            (TokenType::Rbrace, "}", 13, 1),
            (TokenType::Integer, "10", 15, 1),
            (TokenType::Eq, "==", 15, 4),
            (TokenType::Integer, "10", 15, 7),
            (TokenType::Semicolon, ";", 15, 9),
            (TokenType::Integer, "10", 16, 1),
            (TokenType::NotEq, "!=", 16, 4),
            (TokenType::Integer, "9", 16, 7),
            (TokenType::Semicolon, ";", 16, 8),
            (TokenType::Increment, "++", 18, 1),
            (TokenType::Ident, "a1", 18, 3),
            (TokenType::Bang, "!", 20, 1),
            (TokenType::Minus, "-", 20, 2),
            (TokenType::Slash, "/", 20, 3),
            (TokenType::Asterisk, "*", 20, 4),
            (TokenType::Percent, "%", 20, 5),
            (TokenType::Ampersand, "&", 20, 6),
            (TokenType::Integer, "5", 20, 7),
            (TokenType::Ident, "x", 22, 1),
            (TokenType::Assign, "=", 22, 3),
            (TokenType::Integer, "10", 22, 5),
            (TokenType::Ident, "a", 23, 1),
            (TokenType::PlusAssign, "+=", 23, 3),
            (TokenType::Integer, "5", 23, 6),
            (TokenType::Ident, "a", 24, 1),
            (TokenType::MinusAssign, "-=", 24, 3),
            (TokenType::Integer, "5", 24, 6),
            (TokenType::Ident, "a", 25, 1),
            (TokenType::AsteriskAssign, "*=", 25, 3),
            (TokenType::Integer, "5", 25, 6),
            (TokenType::Ident, "a", 26, 1),
            (TokenType::SlashAssign, "/=", 26, 3),
            (TokenType::Integer, "5", 26, 6),
            (TokenType::Ident, "a", 27, 1),
            (TokenType::PercentAssign, "%=", 27, 3),
            (TokenType::Integer, "5", 27, 6),
            (TokenType::If, "if", 29, 1),
            (TokenType::Lparem, "(", 29, 4),
            (TokenType::Integer, "5", 29, 5),
            (TokenType::Lt, "<", 29, 7),
            (TokenType::Integer, "10", 29, 9),
            (TokenType::Rparem, ")", 29, 11),
            (TokenType::Lbrace, "{", 29, 13),
            (TokenType::Return, "return", 30, 5),
            (TokenType::Integer, "1", 30, 12),
            (TokenType::Semicolon, ";", 30, 13),
            (TokenType::Rbrace, "}", 31, 1),
            (TokenType::Else, "else", 31, 3),
            (TokenType::Lbrace, "{", 31, 8),
            (TokenType::Return, "return", 32, 5),
            (TokenType::Integer, "0", 32, 12),
            (TokenType::Semicolon, ";", 32, 13),
            (TokenType::Rbrace, "}", 33, 1),
            (TokenType::Switch, "switch", 34, 1),
            (TokenType::Lparem, "(", 34, 8),
            (TokenType::Ident, "x", 34, 9),
            (TokenType::Rparem, ")", 34, 10),
            (TokenType::Lbrace, "{", 34, 12),
            (TokenType::Case, "case", 35, 5),
            (TokenType::Integer, "1", 35, 10),
            (TokenType::Colon, ":", 35, 11),
            (TokenType::Return, "return", 36, 9),
            (TokenType::Integer, "1", 36, 16),
            (TokenType::Semicolon, ";", 36, 17),
            (TokenType::Default, "default", 37, 5),
            (TokenType::Colon, ":", 37, 12),
            (TokenType::Return, "return", 38, 9),
            (TokenType::Integer, "0", 38, 16),
            (TokenType::Semicolon, ";", 38, 17),
            (TokenType::Rbrace, "}", 39, 1),
            (TokenType::While, "while", 41, 1),
            (TokenType::Lparem, "(", 41, 7),
            (TokenType::Ident, "x", 41, 8),
            (TokenType::Lt, "<", 41, 10),
            (TokenType::Integer, "10", 41, 12),
            (TokenType::Rparem, ")", 41, 14),
            (TokenType::Continue, "continue", 41, 16),
            (TokenType::Semicolon, ";", 41, 24),
            (TokenType::Do, "do", 43, 1),
            (TokenType::Lbrace, "{", 43, 4),
            (TokenType::Return, "return", 44, 5),
            (TokenType::Integer, "1", 44, 12),
            (TokenType::Semicolon, ";", 44, 13),
            (TokenType::Rbrace, "}", 45, 1),
            (TokenType::While, "while", 45, 3),
            (TokenType::Lparem, "(", 45, 9),
            (TokenType::Ident, "x", 45, 10),
            (TokenType::Lt, "<", 45, 12),
            (TokenType::Integer, "10", 45, 14),
            (TokenType::Rparem, ")", 45, 16),
            (TokenType::Semicolon, ";", 45, 17),
            (TokenType::For, "for", 47, 1),
            (TokenType::Lparem, "(", 47, 5),
            (TokenType::Semicolon, ";", 47, 6),
            (TokenType::Semicolon, ";", 47, 7),
            (TokenType::Rparem, ")", 47, 8),
            (TokenType::Increment, "++", 47, 10),
            (TokenType::Ident, "a", 47, 12),
            (TokenType::Semicolon, ";", 47, 13),
            (TokenType::Lbracket, "[", 48, 1),
            (TokenType::Integer, "1", 48, 2),
            (TokenType::Comma, ",", 48, 3),
            (TokenType::Integer, "2", 48, 5),
            (TokenType::Rbracket, "]", 48, 6),
            (TokenType::Struct, "struct", 49, 1),
            (TokenType::Ident, "a", 49, 8),
            (TokenType::Lbrace, "{", 49, 10),
            (TokenType::Rbrace, "}", 49, 11),
            (TokenType::Semicolon, ";", 49, 12),
            (TokenType::Ident, "point", 50, 1),
            (TokenType::Dot, ".", 50, 6),
            (TokenType::Ident, "x", 50, 7),
            (TokenType::Semicolon, ";", 50, 8),
            (TokenType::Ident, "p", 51, 1),
            (TokenType::Arrow, "->", 51, 2),
            (TokenType::Ident, "x", 51, 4),
            (TokenType::Semicolon, ";", 51, 5),
            (TokenType::Character, "'A'", 52, 1),
            (TokenType::Semicolon, ";", 52, 4),
            (TokenType::Character, "'\\''", 53, 1), // -> '\''
            (TokenType::Semicolon, ";", 53, 5),
            (TokenType::Character, "'\\n'", 54, 1), // -> '\n'
            (TokenType::Semicolon, ";", 54, 5),
            (TokenType::Character, "'\\\\'", 55, 1), // -> '\\'
            (TokenType::Semicolon, ";", 55, 5),
            (TokenType::String, "\"Hello, World!\"", 56, 1),
            (TokenType::Semicolon, ";", 56, 16),
            (TokenType::String, "\" escape \\\" \\n \"", 57, 1), // -> " escape \" \n"
            (TokenType::Semicolon, ";", 57, 17),
        ];

        let mut l = Lexer::new(input);

        for (i, t) in tests.iter().enumerate() {
            let tok = l.next_token();
            assert_eq!(
                tok.token_type, t.0,
                "index={}, actual_literal='{}'",
                i, tok.raw_string
            );
            assert_eq!(tok.raw_string, t.1);
            assert_eq!(tok.row, t.2,
                "index={}, actual_row='{}'",
                i, tok.row
            );
            assert_eq!(tok.col, t.3,
                "index={}, actual_literal='{}', actual_col='{}'",
                i, tok.raw_string, tok.col
            );
        }
    }
}
