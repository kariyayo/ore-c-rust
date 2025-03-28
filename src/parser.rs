use std::collections::HashMap;
use crate::lexer::{self, token::TokenType};

mod ast;
use ast::Expression;

#[derive(Debug)]
struct Error {
    errors: Vec<String>,
}

type Result<T> = std::result::Result<T, Error>;

/// Pratt構文解析器
/// 
/// 構文解析関数を文法ルールに関連づけるのではなく、単一のトークンタイプに関連づける。
/// それぞれのトークンタイプに対して、中置演算子と前置演算子と、2つの構文解析関数を関連づける。
struct Parser {
    l: lexer::Lexer,

    /// 現在調べているトークン
    cur_token: lexer::token::Token,

    /// curTokenの次のトークン
    peek_token: lexer::token::Token,

    errors: Vec<String>,

    precedences: HashMap<TokenType, ExpressionPrecedence>,
}

// 式の優先順位
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ExpressionPrecedence {
    Lowest,
    Equals, // ==
    LessGreater, // > または <
    Sum, // +
    Product, // *
    Prefix, // -X または !X
    Call, // myFunction(X)
}

fn is_infix_token_type(token_type: TokenType) -> bool {
    return match token_type {
        TokenType::Plus | TokenType::Minus | TokenType::Slash | TokenType::Asterisk | TokenType::Eq | TokenType::NotEq | TokenType::Lt | TokenType::Gt => {
            true
        }
        _ => {
            false
        }
    };
}

impl Parser {
    pub(crate) fn new(l: lexer::Lexer) -> Parser {
        let mut precedences: HashMap<TokenType, ExpressionPrecedence> = HashMap::with_capacity(8);
        precedences.insert(TokenType::Eq, ExpressionPrecedence::Equals);
        precedences.insert(TokenType::NotEq, ExpressionPrecedence::Equals);
        precedences.insert(TokenType::Lt, ExpressionPrecedence::LessGreater);
        precedences.insert(TokenType::Gt, ExpressionPrecedence::LessGreater);
        precedences.insert(TokenType::Plus, ExpressionPrecedence::Sum);
        precedences.insert(TokenType::Minus, ExpressionPrecedence::Sum);
        precedences.insert(TokenType::Slash, ExpressionPrecedence::Product);
        precedences.insert(TokenType::Asterisk, ExpressionPrecedence::Product);
        let mut p = Parser {
            l: l,
            cur_token: lexer::token::Token { token_type: TokenType::Eof, literal: "".to_string() },
            peek_token: lexer::token::Token { token_type: TokenType::Eof, literal: "".to_string() },
            errors: vec![],
            precedences: precedences,
        };

        p.next_token();
        p.next_token();
        return p;
    }

    pub(crate) fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { statements: vec![] };
        while self.cur_token.token_type != TokenType::Eof {
            let stmt = self.parse_statement();
            match stmt {
                Ok(s) => {
                    program.statements.push(s);
                }
                Err(e) => {
                    for msg in e.errors.iter() {
                        self.errors.push(msg.to_string());
                    }
                    panic!("parse error: \n{}", self.errors.join("\n"));
                }
            }
            self.next_token();
        }
        if self.errors.len() > 0 {
            panic!("parse error: \n{}", self.errors.join("\n"));
        }
        return program
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
        println!("#### current token_type:{:?}, literal:{}, peek token_type:{:?} ####", self.cur_token.token_type, self.cur_token.literal, self.peek_token.token_type);
    }

    // 文をパースする
    fn parse_statement(&mut self) -> Result<ast::Statement> {
        let result = match self.cur_token.token_type {
            TokenType::Return => {
                self.parse_return_statement()
            }
            TokenType::Int => {
                self.parse_vardecl_statement()
            }
            TokenType::Lbrace => {
                self.parse_block_statement()
            }
            TokenType::If => {
                self.parse_if_statement()
            }
            _ => {
                self.parse_expression_statement()
            }
        };
        return result
    }

    // return <expression>;
    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        self.next_token();
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(ast::Statement::Return { value: None });
        } else {
            let value = self.parse_expression(ExpressionPrecedence::Lowest)?;
            let result = ast::Statement::Return { value: Some(value) };
            self.next_token();
            return if self.cur_token.token_type == TokenType::Semicolon {
                Ok(result)
            } else {
                let error_msg = format!("[parse_return_statement] expected next token to be SEMICOLON, got {:?}", self.cur_token.token_type);
                Err(Error { errors: vec![error_msg] })
            };
        };
    }

    // <type_ref> <ident> = <expression>;
    // <type_ref> <ident>;
    fn parse_vardecl_statement(&mut self) -> Result<ast::Statement> {
        let type_decl = ast::TypeRef { type_name: self.cur_token.literal.to_string() };
        if self.peek_token.token_type == TokenType::Ident {
            self.next_token();
        } else {
            return Err(Error { errors: vec![format!("expected next token to be IDENT, got {:?}", self.cur_token.token_type)] });
        }
        let name = self.cur_token.literal.clone();

        let result = if self.peek_token.token_type != TokenType::Assign {
            ast::Statement::VarDecl { type_dec: type_decl, name, value: None }
        } else {
            self.next_token(); // `=` を読み飛ばす
            self.next_token();
            let value = self.parse_expression(ExpressionPrecedence::Lowest)?;
            ast::Statement::VarDecl { type_dec: type_decl, name, value: Some(value) }
        };
        self.next_token();
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(result);
        } else {
            let error_msg = format!("[parse_vardecl_statement] expected next token to be SEMICOLON, got {:?}", self.cur_token.token_type);
            return Err(Error { errors: vec![error_msg] });
        }
    }

    // { <statement>* }
    fn parse_block_statement(&mut self) -> Result<ast::Statement> {
        self.next_token();
        let mut statements = vec![];
        while self.cur_token.token_type != TokenType::Rbrace && self.peek_token.token_type != TokenType::Eof {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            if self.peek_token.token_type == TokenType::Illegal {
                return Err(Error { errors: vec![format!("next token is Illegal, got {:?}", self.peek_token.token_type)] });
            }
            self.next_token();
        }
        if self.cur_token.token_type == TokenType::Eof {
            return Ok(ast::Statement::Block { statements });
        } else if self.cur_token.token_type == TokenType::Rbrace {
            return Ok(ast::Statement::Block { statements });
        } else {
            let error_msg = format!("expected next token to be Rbrace, got {:?}", self.cur_token.token_type);
            return Err(Error { errors: vec![error_msg] });
        }
    }

    // if (<expression>) <block_statement>
    // if (<expression>) <block_statement> else <block_statement>
    fn parse_if_statement(&mut self) -> Result<ast::Statement> {
        self.next_token();
        if self.cur_token.token_type == TokenType::Lparem {
            self.next_token();
        } else {
            return Err(Error { errors: vec![format!("expected next token to be Lparem, got {:?}", self.cur_token.token_type)] });
        }
        let condition = self.parse_expression(ExpressionPrecedence::Lowest)?;
        self.next_token();
        if self.cur_token.token_type == TokenType::Rparem {
            self.next_token();
        } else {
            return Err(Error { errors: vec![format!("expected next token to be Rparem, got {:?}", self.cur_token.token_type)] });
        }
        let consequence = self.parse_statement()?;
        if self.peek_token.token_type != TokenType::Else {
            return Ok(ast::Statement::If { condition, consequence: Box::new(consequence), alternative: None });
        } else {
            self.next_token(); // read `}` or `;`
            self.next_token(); // read `else`
            let alternative = self.parse_statement()?;
            return Ok(ast::Statement::If { condition, consequence: Box::new(consequence), alternative: Some(Box::new(alternative)) });
        }
    }

    // <expression>;
    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        let expression = self.parse_expression(ExpressionPrecedence::Lowest);
        let result = expression.map(|exp| ast::Statement::ExpressionStatement { expression: exp })?;
        self.next_token();
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(result);
        } else {
            let error_msg = format!("[parse_expression_statement] expected next token to be SEMICOLON, got {:?}", self.cur_token.token_type);
            return Err(Error { errors: vec![error_msg] });
        }
    }

    // ======================
    // ここから式に関する処理
    // ======================

    // 現在のトークンの次のトークン優先順位を返す
    fn peek_precedence(&self) -> ExpressionPrecedence {
        return *self.precedences.get(&self.peek_token.token_type).unwrap_or(&ExpressionPrecedence::Lowest);
    }

    // 現在のトークンの優先順位を返す
    fn cur_precedence(&self) -> ExpressionPrecedence {
        return *self.precedences.get(&self.cur_token.token_type).unwrap_or(&ExpressionPrecedence::Lowest);
    }

    // 式をパースする
    fn parse_expression(&mut self, precedence: ExpressionPrecedence) -> Result<ast::Expression> {
        // まず、前置演算子もしくはリテラルをパースする
        let prefix_result = self.prefix();
        match prefix_result {
            None => {
                return Err(Error { errors: vec![format!("no prefix parse function for {:?}", self.cur_token.token_type)] });
            }
            Some(Err(e)) => {
                return Err(e);
            }
            Some(Ok(exp)) => {
                let mut result = exp;
                loop {
                    // 次のトークンがセミコロンの場合は文が終了するので、ここでparse結果を返す。
                    // もしくは、
                    // 次のトークンを現在の演算子よりも優先する場合は、ここでparse結果を返す。
                    // 例えば、`5 * 5 + 3` の場合、`5 * 5` が終わった時点で `*` の優先順位（ `precedence` の値）が
                    // 次の演算子 `+` の優先順位（ `self.peek_precedence` の値）より高いので `5 * 5` のparse結果を返す。
                    if self.peek_token.token_type == TokenType::Semicolon || precedence >= self.peek_precedence() {
                        return Ok(result);
                    }

                    if !is_infix_token_type(self.peek_token.token_type) {
                        return Ok(result);
                    }

                    let left_exp = result;
                    self.next_token();

                    let infix_result = self.parse_infix_expression(left_exp);
                    result = infix_result?;
                }
            }
        }
    }

    fn prefix(&mut self) -> Option<Result<ast::Expression>> {
        let current_token_type = self.cur_token.token_type;
        let result = match current_token_type {
            TokenType::Ident => {
                Some(self.parse_identifier())
            }
            TokenType::Integer => {
                Some(self.parse_integer_literal())
            }
            TokenType::Bang | TokenType::Minus | TokenType::Increment | TokenType::Decrement => {
                Some(self.parse_prefix_expression())
            }
            TokenType::Lparem => {
                Some(self.parse_grouped_expression())
            }
            _ => {
                None
            }
        };
        return result;
    }

    fn parse_identifier(&self) -> Result<ast::Expression> {
        return Ok(ast::Expression::Identifier { value: self.cur_token.literal.clone() });
    }

    fn parse_integer_literal(&self) -> Result<ast::Expression> {
        return self.cur_token.literal.parse()
            .map(|value| ast::Expression::Int { value })
            .map_err(|_| Error { errors: vec!["parse int error".to_string()] });
    }

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression> {
        self.next_token();
        let exp = self.parse_expression(ExpressionPrecedence::Lowest);
        if self.peek_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("expected next token to be Rparem, got {:?}", self.peek_token.token_type)] });
        }
        self.next_token();
        return exp;
    }

    // !, -, ++, -- の前置演算子をパースする
    fn parse_prefix_expression(&mut self) -> Result<ast::Expression> {
        let operator = self.cur_token.literal.clone();
        self.next_token();
        return self.parse_expression(ExpressionPrecedence::Prefix)
            .map(|right|
                ast::Expression::PrefixExpression {
                    operator,
                    right: Box::new(right),
                }
            );
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<ast::Expression> {
        let operator = self.cur_token.literal.clone();
        let operator_precedence = self.cur_precedence();
        self.next_token();
        return self.parse_expression(operator_precedence)
            .map(|right|
                ast::Expression::InfixExpression {
                    operator,
                    left: Box::new(left),
                    right: Box::new(right),
                }
            );
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::Expression;

    use super::*;

    #[test]
    fn test_vardecl() {
        // given
        let input = "
int five = 5;
int x = 10;
";
        let expected = vec![
            ("int", "five", 5),
            ("int", "x", 10),
        ];

        // when
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        // then
        assert_eq!(program.statements.len(), 2);
        for (i, stmt) in program.statements.iter().enumerate() {
            let (expected_type, expected_name, expected_value) = expected[i];
            match stmt {
                ast::Statement::VarDecl { type_dec, name, value } => {
                    assert_eq!(type_dec, &ast::TypeRef { type_name: expected_type.to_string() });
                    assert_eq!(name, expected_name);
                    assert_eq!(value, &Some(Expression::Int { value: expected_value }));
                }
                _ => panic!("Statement is not VarDecl"),
            }
        }
    }

    #[test]
    fn test_return() {
        // given
        let input = "
return 5;
return 9876;
";
        let expected = vec![5, 9876];

        // when
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let program = p.parse_program();

        // then
        assert_eq!(program.statements.len(), 2);
        for (i, stmt) in program.statements.iter().enumerate() {
            let expected_value= expected[i];
            match stmt {
                ast::Statement::Return { value } => {
                    assert_eq!(value, &Some(Expression::Int { value: expected_value }));
                }
                _ => panic!("Statement is not Return"),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        // given
        let input = "
foobar;
";
        let expected = vec!["foobar"];

        // when
        let mut p = Parser::new(lexer::Lexer::new(input));
        let program = p.parse_program();

        // then
        assert_eq!(program.statements.len(), 1);
        for (i, stmt) in program.statements.iter().enumerate() {
            let expected_value= expected[i];
            match stmt {
                ast::Statement::ExpressionStatement { expression } => {
                    assert_eq!(expression, &Expression::Identifier { value: expected_value.to_string() });
                }
                _ => panic!("Statement is not Return"),
            }
        }
    }

    #[test]
    fn test_prefix_expression() {
        // given
        let input = "
!foobar;
-5;
++a;
";
        let expected = vec![
            ("!", ast::Expression::Identifier { value: "foobar".to_string() }),
            ("-", ast::Expression::Int { value: 5 }),
            ("++", ast::Expression::Identifier { value: "a".to_string() }),
        ];

        // when
        let mut p = Parser::new(lexer::Lexer::new(input));
        let program = p.parse_program();

        // then
        assert_eq!(program.statements.len(), 3);
        for (i, stmt) in program.statements.iter().enumerate() {
            let (expected_operator, expected_right)= &expected[i];
            match stmt {
                ast::Statement::ExpressionStatement { expression: ast::Expression::PrefixExpression { operator, right } } => {
                    assert_eq!(operator, expected_operator);
                    assert_eq!(right.as_ref(), expected_right);
                }
                _ => panic!("Statement is not Return"),
            }
        }
    }

    #[test]
    fn test_infix_expression() {
        // given
        let input = "
5 + 6;
5 - 6;
5 * 6;
5 / 6;
5 == 6;
5 != 6;
5 > 6;
5 < 6;
";
        let expected = vec![
            ("+", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("-", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("*", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("/", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("==", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("!=", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            (">", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("<", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
        ];

        // when
        let mut p = Parser::new(lexer::Lexer::new(input));
        let program = p.parse_program();

        // then
        assert_eq!(program.statements.len(), 8);
        for (i, stmt) in program.statements.iter().enumerate() {
            let (expected_operator, expected_left, expected_right)= &expected[i];
            match stmt {
                ast::Statement::ExpressionStatement { expression: ast::Expression::InfixExpression { operator, left, right } } => {
                    assert_eq!(operator, expected_operator);
                    assert_eq!(left.as_ref(), expected_left);
                    assert_eq!(right.as_ref(), expected_right);
                }
                _ => panic!("Statement is not Return"),
            }
        }
    }

    #[test]
    fn test_operator_precedence() {
        // given
        let tests = [
            ("a + b;", "(a + b);"),
            ("a + b - c;", "((a + b) - c);"),
            ("a + b * c;", "(a + (b * c));"),
            ("a + b / c;", "(a + (b / c));"),
            ("a + b * c + d;", "((a + (b * c)) + d);"),
            ("a + b * c + d / e - f;", "(((a + (b * c)) + (d / e)) - f);"),
            ("-5 * 5;", "((-5) * 5);"),
            ("5 > 4 == 3 < 4;", "((5 > 4) == (3 < 4));"),
            ("5 < 4 != 3 > 4;", "((5 < 4) != (3 > 4));"),
            ("3 + 4 * 5 == 3 * 1 + 4 * 5;", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)));"),
            ("1 + (2 + 3) + 4;", "((1 + (2 + 3)) + 4);"),
            ("(5 + 5) * 2;", "((5 + 5) * 2);"),
            ("2 / (5 + 5);", "(2 / (5 + 5));"),
            ("-(5 + 5);", "(-(5 + 5));"),
        ];
        for (input, expected) in tests.iter() {
            // when
            let mut p = Parser::new(lexer::Lexer::new(input));
            let program = p.parse_program();

            // then
            assert_eq!(program.statements.len(), 1);
            let stmt = program.statements.first().unwrap();
            assert_eq!(expected.to_string(), stmt.to_string());
        }
    }

    #[test]
    fn test_if_statement() {
        // given
        let input = "
if (x < y) { x; y; }
if (x < y) { x; y; } else { aaa; }
if (x < y) { x; y; } else aaa;
if (x < y) x + 2; else y;
";

        let expected = vec![
            ("(x < y)", "{\n    x;\n    y;\n}", None),
            ("(x < y)", "{\n    x;\n    y;\n}", Some("{\n    aaa;\n}")),
            ("(x < y)", "{\n    x;\n    y;\n}", Some("aaa;")),
            ("(x < y)", "(x + 2);", Some("y;")),
        ];

        // when
        let mut p = Parser::new(lexer::Lexer::new(input));
        let program = p.parse_program();

        // then
        assert_eq!(program.statements.len(), 4);
        for (i, stmt) in program.statements.iter().enumerate() {
            let (expected_condition, expected_consequence, expected_alternative) = expected[i];
            match stmt {
                ast::Statement::If { condition, consequence, alternative } => {
                    assert_eq!(condition.to_string(), expected_condition.to_string());
                    assert_eq!(consequence.to_string(), expected_consequence.to_string());
                    assert_eq!(alternative.as_ref().map(|a| a.to_string()), expected_alternative.map(|a| a.to_string()));
                }
                _ => panic!("Statement is not Return"),
            }
        }
    }
}
