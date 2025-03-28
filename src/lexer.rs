use token::TokenType;

pub(crate) mod token;

pub(crate) struct Lexer {
    input: String,
    position: usize,      // 現在の位置
    read_position: usize, // これから読み込む位置（現在の文字の次）
    ch: char,             // 何も読んでない or ファイルの終わりを示す場合はNULL文字になる
}

impl Lexer {
    pub(crate) fn new(input: &str) -> Lexer {
        let mut l = Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: '\u{0}',
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
    }

    fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }

    fn is_letter(&self, ch: char) -> bool {
        return 'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_';
    }

    fn read_identifier(&mut self) -> String {
        let start_pos = self.position;
        while self.is_letter(self.ch) {
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

    pub(crate) fn next_token(&mut self) -> token::Token {
        self.skip_whitespace();
        let (tok, skip_read) = match self.ch {
            ',' => (
                token::Token {
                    token_type: TokenType::Comma,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            ';' => (
                token::Token {
                    token_type: TokenType::Semicolon,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    (
                        token::Token {
                            token_type: TokenType::Eq,
                            literal: "==".to_string(),
                        },
                        false,
                    )
                } else {
                    (
                        token::Token {
                            token_type: TokenType::Assign,
                            literal: self.ch.to_string(),
                        },
                        false,
                    )
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    (
                        token::Token {
                            token_type: TokenType::NotEq,
                            literal: "!=".to_string(),
                        },
                        false,
                    )
                } else {
                    (
                        token::Token {
                            token_type: TokenType::Bang,
                            literal: self.ch.to_string(),
                        },
                        false,
                    )
                }
            }
            '+' => {
                if self.peek_char() == '+' {
                    self.read_char();
                    (
                        token::Token {
                            token_type: TokenType::Increment,
                            literal: "++".to_string(),
                        },
                        false,
                    )
                } else {
                    (
                        token::Token {
                            token_type: TokenType::Plus,
                            literal: self.ch.to_string(),
                        },
                        false,
                    )
                }
            },
            '-' => {
                if self.peek_char() == '-' {
                    self.read_char();
                    (
                        token::Token {
                            token_type: TokenType::Decrement,
                            literal: "--".to_string(),
                        },
                        false,
                    )
                } else {
                    (
                        token::Token {
                            token_type: TokenType::Minus,
                            literal: self.ch.to_string(),
                        },
                        false,
                    )
                }
            },
            '*' => (
                token::Token {
                    token_type: TokenType::Asterisk,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '/' => (
                token::Token {
                    token_type: TokenType::Slash,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '<' => (
                token::Token {
                    token_type: TokenType::Lt,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '>' => (
                token::Token {
                    token_type: TokenType::Gt,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '(' => (
                token::Token {
                    token_type: TokenType::Lparem,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            ')' => (
                token::Token {
                    token_type: TokenType::Rparem,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '{' => (
                token::Token {
                    token_type: TokenType::Lbrace,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '}' => (
                token::Token {
                    token_type: TokenType::Rbrace,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '[' => (
                token::Token {
                    token_type: TokenType::Lbracket,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            ']' => (
                token::Token {
                    token_type: TokenType::Rbracket,
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '\u{0}' => (
                token::Token {
                    token_type: TokenType::Eof,
                    literal: "".to_string(),
                },
                false,
            ),
            _ => {
                if self.is_letter(self.ch) {
                    let literal = self.read_identifier();
                    let token_type = token::lookup_ident(literal.as_str());
                    (
                        token::Token {
                            token_type,
                            literal,
                        },
                        true,
                    )
                } else if self.is_digit(self.ch) {
                    let literal = self.read_number();
                    (
                        token::Token {
                            token_type: TokenType::Integer,
                            literal,
                        },
                        true,
                    )
                } else {
                    (
                        token::Token {
                            token_type: TokenType::Illegal,
                            literal: self.ch.to_string(),
                        },
                        false,
                    )
                }
            }
        };
        if !skip_read {
            self.read_char();
        }
        return tok;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let input = "int five = 5;

int add(int x, int y) {
    return x + y;
}

int main(int argc, char *argv[]) {
    int ten = 10;
    int result = add(five, ten);
}

10 == 10;
10 != 9;

++a

!-/*5

if (5 < 10) {
    return true;
} else {
    return false;
}
";
        let tests = vec![
            (TokenType::Int, "int"),
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Integer, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "int"),
            (TokenType::Ident, "add"),
            (TokenType::Lparem, "("),
            (TokenType::Int, "int"),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Int, "int"),
            (TokenType::Ident, "y"),
            (TokenType::Rparem, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Int, "int"),
            (TokenType::Ident, "main"),
            (TokenType::Lparem, "("),
            (TokenType::Int, "int"),
            (TokenType::Ident, "argc"),
            (TokenType::Comma, ","),
            (TokenType::Char, "char"),
            (TokenType::Asterisk, "*"),
            (TokenType::Ident, "argv"),
            (TokenType::Lbracket, "["),
            (TokenType::Rbracket, "]"),
            (TokenType::Rparem, ")"),
            (TokenType::Lbrace, "{"),
            (TokenType::Int, "int"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Integer, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "int"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::Lparem, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::Rparem, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Rbrace, "}"),
            (TokenType::Integer, "10"),
            (TokenType::Eq, "=="),
            (TokenType::Integer, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Integer, "10"),
            (TokenType::NotEq, "!="),
            (TokenType::Integer, "9"),
            (TokenType::Semicolon, ";"),
            (TokenType::Increment, "++"),
        ];

        let mut l = Lexer::new(input);

        for (i, t) in tests.iter().enumerate() {
            let tok = l.next_token();
            assert_eq!(
                tok.token_type, t.0,
                "index={}, actual_literal='{}'",
                i, tok.literal
            );
            assert_eq!(tok.literal, t.1);
        }
    }
}
