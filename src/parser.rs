use std::{collections::HashMap, sync::OnceLock};

use ast::Expression;

use crate::lexer::{self, token::TokenType};

mod ast;

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

impl Parser {
    pub(crate) fn new(l: lexer::Lexer) -> Parser {
        let mut precedences: HashMap<TokenType, ExpressionPrecedence> = HashMap::with_capacity(8);
        precedences.insert(lexer::token::EQ, ExpressionPrecedence::Equals);
        precedences.insert(lexer::token::NOT_EQ, ExpressionPrecedence::Equals);
        precedences.insert(lexer::token::LT, ExpressionPrecedence::LessGreater);
        precedences.insert(lexer::token::GT, ExpressionPrecedence::LessGreater);
        precedences.insert(lexer::token::PLUS, ExpressionPrecedence::Sum);
        precedences.insert(lexer::token::MINUS, ExpressionPrecedence::Sum);
        precedences.insert(lexer::token::SLASH, ExpressionPrecedence::Product);
        precedences.insert(lexer::token::ASTERISK, ExpressionPrecedence::Product);
        let mut p = Parser {
            l: l,
            cur_token: lexer::token::Token { token_type: lexer::token::EOF, literal: "".to_string() },
            peek_token: lexer::token::Token { token_type: lexer::token::EOF, literal: "".to_string() },
            errors: vec![],
            precedences: precedences,
        };

        p.next_token();
        p.next_token();
        return p;
    }

    pub(crate) fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program { statements: vec![] };
        while self.cur_token.token_type != lexer::token::EOF {
            if self.cur_token.token_type != lexer::token::SEMICOLON {
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
        println!("#### token_type:{}, literal:{} ####", self.cur_token.token_type, self.cur_token.literal);
    }

    // 文をパースする
    fn parse_statement(&mut self) -> Result<ast::Statement> {
        let result = match self.cur_token.token_type {
            lexer::token::RETURN => {
                self.parse_return_statement()
            }
            lexer::token::INT => {
                self.parse_vardecl_statement()
            }
            _ => {
                self.parse_expression_statement()
            }
        };
        self.next_token();
        let s = &self.cur_token.literal;
        if s != lexer::token::SEMICOLON {
            let error_msg = format!("expected next token to be SEMICOLON, got {}", self.cur_token.token_type);
            return if result.is_err() {
                let mut errors = result.err().unwrap().errors.clone();
                errors.push(error_msg);
                Err(Error { errors })
            } else {
                Err(Error { errors: vec![error_msg] })
            }
        }
        return result
    }

    // return <expression>;
    fn parse_return_statement(&mut self) -> Result<ast::Statement> {
        self.next_token();
        let value = self.parse_expression(ExpressionPrecedence::Lowest);
        return value.map(|v| ast::Statement::Return { value: Some(v) });
        // return Ok(ast::Statement::Return { value: Some(value) });
    }

    // <type_ref> <ident> = <expression>;
    // <type_ref> <ident>;
    fn parse_vardecl_statement(&mut self) -> Result<ast::Statement> {
        let type_decl = self.parse_type(self.cur_token.literal.as_str());
        if self.peek_token.token_type == lexer::token::IDENT {
            self.next_token();
        } else {
            return Err(Error { errors: vec![format!("expected next token to be IDENT, got {}", self.cur_token.token_type)] });
        }
        let name = self.cur_token.literal.clone();

        if self.peek_token.token_type != lexer::token::ASSIGN {
            return Ok(ast::Statement::VarDecl { type_dec: type_decl, name, value: None });
        } else {
            self.next_token(); // `=` を読み飛ばす
            self.next_token();
            let value = self.parse_expression(ExpressionPrecedence::Lowest);
            return value.map(|v| ast::Statement::VarDecl { type_dec: type_decl, name, value: Some(v) });
        }
    }

    // <expression>;
    fn parse_expression_statement(&mut self) -> Result<ast::Statement> {
        let expression = self.parse_expression(ExpressionPrecedence::Lowest);
        return expression.map(|exp| ast::Statement::ExpressionStatement { expression: exp });
    }

    fn parse_type(&self, type_name: &str) -> ast::TypeRef {
        let type_node = ast::TypeRef { type_name: type_name.to_string() };
        return type_node;
    }

    // 現在のトークンの次のトークン優先順位を返す
    fn peek_precedence(&self) -> ExpressionPrecedence {
        return *self.precedences.get(self.peek_token.token_type).unwrap_or(&ExpressionPrecedence::Lowest);
    }

    // 現在のトークンの優先順位を返す
    fn cur_precedence(&self) -> ExpressionPrecedence {
        return *self.precedences.get(self.cur_token.token_type).unwrap_or(&ExpressionPrecedence::Lowest);
    }

    // 式をパースする
    fn parse_expression(&mut self, precedence: ExpressionPrecedence) -> Result<ast::Expression> {
        let current_token_type = self.cur_token.token_type;
        let prefix = match current_token_type {
            lexer::token::IDENT => {
                Some(self.parse_identifier())
            }
            lexer::token::INTEGER => {
                Some(self.parse_integer_literal())
            }
            lexer::token::BANG | lexer::token::MINUS | lexer::token::INCREMENT | lexer::token::DECREMENT => {
                Some(self.parse_prefix_expression())
            }
            _ => None
        };
        return match prefix {
            Some(Ok(exp)) => {
                let mut result: Option<Result<Expression>> = None;
                let mut left_exp = exp;
                loop {
                    // 次のトークンがセミコロンの場合は文が終了するので、ここでparse結果を返す。
                    // もしくは、
                    // 次のトークンを現在の演算子よりも優先する場合は、ここでparse結果を返す。
                    // 例えば、`5 * 5 + 3` の場合、`5 * 5` が終わった時点で `*` の優先順位（ `precedence` の値）が
                    // 次の演算子 `+` の優先順位（ `self.peek_precedence` の値）より高いので `5 * 5` のparse結果を返す。
                    if self.peek_token.token_type == lexer::token::SEMICOLON || precedence >= self.peek_precedence() {
                        result = Some(Ok(left_exp));
                        break;
                    }
                    let peek_token_type = self.peek_token.token_type;
                    match peek_token_type {
                        lexer::token::PLUS | lexer::token::MINUS | lexer::token::SLASH | lexer::token::ASTERISK | lexer::token::EQ | lexer::token::NOT_EQ | lexer::token::LT | lexer::token::GT => {
                            self.next_token();
                            let infix = self.parse_infix_expression(left_exp);
                            if infix.is_err() {
                                // TODO: エラーだったら parse_infix_expression にmoveしたleft_expを戻してもらいたい
                                result = Some(infix);
                                break;
                            }
                            left_exp = infix.ok().unwrap();
                        }
                        _ => {
                            break;
                        }
                    }
                }
                return result.unwrap()
            }
            Some(Err(e)) => {
                Err(e)
            }
            None => {
                Err(Error { errors: vec![format!("no prefix parse function for {}", current_token_type)] })
            }
        };
    }

    fn parse_identifier(&self) -> Result<ast::Expression> {
        return Ok(ast::Expression::Identifier { value: self.cur_token.literal.clone() });
    }

    fn parse_integer_literal(&self) -> Result<ast::Expression> {
        return self.cur_token.literal.parse()
            .map(|value| ast::Expression::Int { value })
            .map_err(|_| Error { errors: vec!["parse int error".to_string()] });
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
}
