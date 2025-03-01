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
                    token_type: token::COMMA.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            ';' => (
                token::Token {
                    token_type: token::SEMICOLON.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    (
                        token::Token {
                            token_type: token::EQ.to_string(),
                            literal: "==".to_string(),
                        },
                        false,
                    )
                } else {
                    (
                        token::Token {
                            token_type: token::ASSIGN.to_string(),
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
                            token_type: token::NOT_EQ.to_string(),
                            literal: "!=".to_string(),
                        },
                        false,
                    )
                } else {
                    (
                        token::Token {
                            token_type: token::BANG.to_string(),
                            literal: self.ch.to_string(),
                        },
                        false,
                    )
                }
            }
            '+' => (
                token::Token {
                    token_type: token::PLUS.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '-' => (
                token::Token {
                    token_type: token::MINUS.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '*' => (
                token::Token {
                    token_type: token::ASTERISK.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '/' => (
                token::Token {
                    token_type: token::SLASH.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '<' => (
                token::Token {
                    token_type: token::LT.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '>' => (
                token::Token {
                    token_type: token::RT.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '(' => (
                token::Token {
                    token_type: token::LPAREM.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            ')' => (
                token::Token {
                    token_type: token::RPAREM.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '{' => (
                token::Token {
                    token_type: token::LBRACE.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '}' => (
                token::Token {
                    token_type: token::RBRACE.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '[' => (
                token::Token {
                    token_type: token::LBRACKET.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            ']' => (
                token::Token {
                    token_type: token::RBRACKET.to_string(),
                    literal: self.ch.to_string(),
                },
                false,
            ),
            '\u{0}' => (
                token::Token {
                    token_type: token::EOF.to_string(),
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
                            token_type: token::INTEGER.to_string(),
                            literal,
                        },
                        true,
                    )
                } else {
                    (
                        token::Token {
                            token_type: token::ILLEGAL.to_string(),
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

!-/*5

if (5 < 10) {
    return true;
} else {
    return false;
}
";
        let tests = vec![
            (token::INT, "int"),
            (token::IDENT, "five"),
            (token::ASSIGN, "="),
            (token::INTEGER, "5"),
            (token::SEMICOLON, ";"),
            (token::INT, "int"),
            (token::IDENT, "add"),
            (token::LPAREM, "("),
            (token::INT, "int"),
            (token::IDENT, "x"),
            (token::COMMA, ","),
            (token::INT, "int"),
            (token::IDENT, "y"),
            (token::RPAREM, ")"),
            (token::LBRACE, "{"),
            (token::RETURN, "return"),
            (token::IDENT, "x"),
            (token::PLUS, "+"),
            (token::IDENT, "y"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            (token::INT, "int"),
            (token::IDENT, "main"),
            (token::LPAREM, "("),
            (token::INT, "int"),
            (token::IDENT, "argc"),
            (token::COMMA, ","),
            (token::CHAR, "char"),
            (token::ASTERISK, "*"),
            (token::IDENT, "argv"),
            (token::LBRACKET, "["),
            (token::RBRACKET, "]"),
            (token::RPAREM, ")"),
            (token::LBRACE, "{"),
            (token::INT, "int"),
            (token::IDENT, "ten"),
            (token::ASSIGN, "="),
            (token::INTEGER, "10"),
            (token::SEMICOLON, ";"),
            (token::INT, "int"),
            (token::IDENT, "result"),
            (token::ASSIGN, "="),
            (token::IDENT, "add"),
            (token::LPAREM, "("),
            (token::IDENT, "five"),
            (token::COMMA, ","),
            (token::IDENT, "ten"),
            (token::RPAREM, ")"),
            (token::SEMICOLON, ";"),
            (token::RBRACE, "}"),
            (token::INTEGER, "10"),
            (token::EQ, "=="),
            (token::INTEGER, "10"),
            (token::SEMICOLON, ";"),
            (token::INTEGER, "10"),
            (token::NOT_EQ, "!="),
            (token::INTEGER, "9"),
            (token::SEMICOLON, ";"),
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
