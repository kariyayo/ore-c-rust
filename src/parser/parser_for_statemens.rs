use super::{Parser, Result, Error, TokenType, ExpressionPrecedence};
use super::ast::{Statement, TypeRef, Declarator, SwitchBlock, SwitchLabel, SwitchLabelEntry};

impl Parser {

    // 文をパースする
    fn parse_statement(&mut self) -> Result<Statement> {
        let result = match self.cur_token.token_type {
            TokenType::Return => {
                self.parse_return_statement()
            }
            TokenType::Int | TokenType::Char | TokenType::Struct => {
                self.parse_vardecl_statement()
            }
            TokenType::Lbrace => {
                self.parse_block_statement()
            }
            TokenType::If => {
                self.parse_if_statement()
            }
            TokenType::Switch => {
                self.parse_switch_statement()
            }
            TokenType::Break => {
                self.parse_break_statement()
            }
            TokenType::While => {
                self.parse_while_statement()
            }
            TokenType::Do => {
                self.parse_dowhile_statement()
            }
            TokenType::For => {
                self.parse_for_statement()
            }
            TokenType::Continue => {
                self.parse_continue_statement()
            }
            _ => {
                self.parse_expression_statement()
            }
        };
        return result
    }

    // return <expression>;
    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.next_token();
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(Statement::Return(None));
        } else {
            let value = self.parse_expression(ExpressionPrecedence::Lowest)?;
            let result = Statement::Return(Some(value));
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
    // <type_ref> <ident>, <ident>, ...;
    // <type_ref> * <ident>;
    // <type_ref> <ident>[<size>];
    // <type_ref> <ident>[] = {<expression>, <expression>, ...};
    //
    // ex) int a, *b, c[10];
    fn parse_vardecl_statement(&mut self) -> Result<Statement> {
        let type_dec = if self.cur_token.token_type == TokenType::Struct {
            self.parse_struct_type()?
        } else {
            let t = TypeRef::Named(self.cur_token.literal());
            self.next_token();
            t
        };
        let declarators: Vec<(TypeRef, Declarator)> =
            if self.cur_token.token_type == TokenType::Semicolon {
                vec![]
            } else {
                self.parse_declarators(&type_dec)?
            };
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(Statement::VarDecl(declarators));
        } else {
            let error_msg = format!("[parse_vardecl_statement] expected next token to be SEMICOLON, got {:?}", self.cur_token.token_type);
            return Err(Error { errors: vec![error_msg] });
        }
    }

    // { <statement>* }
    pub(super) fn parse_block_statement(&mut self) -> Result<Statement> {
        self.next_token();
        let mut statements = vec![];
        while self.cur_token.token_type != TokenType::Rbrace && self.peek_token.token_type != TokenType::Eof {
            let stmt = self.parse_statement()?;
            statements.push(stmt);
            if self.peek_token.token_type == TokenType::Illegal {
                return Err(Error { errors: vec![format!("[parse_block_statement] next token is Illegal, got {:?}", self.peek_token.token_type)] });
            }
            self.next_token();
        }
        if self.cur_token.token_type == TokenType::Eof {
            return Ok(Statement::Block(statements));
        } else if self.cur_token.token_type == TokenType::Rbrace {
            return Ok(Statement::Block(statements));
        } else {
            let error_msg = format!("[parse_block_statement] expected next token to be Rbrace, got {:?}", self.cur_token.token_type);
            return Err(Error { errors: vec![error_msg] });
        }
    }

    // if (<expression>) <block_statement> | <expression_statement>
    // if (<expression>) <block_statement> | <expression_statement> else <block_statement> | <expression_statement>
    fn parse_if_statement(&mut self) -> Result<Statement> {
        self.next_token();
        if self.cur_token.token_type == TokenType::Lparem {
            self.next_token();
        } else {
            return Err(Error { errors: vec![format!("[parse_if_statement] expected next token to be Lparem, got {:?}", self.cur_token.token_type)] });
        }
        let condition = self.parse_expression(ExpressionPrecedence::Lowest)?;
        self.next_token();
        if self.cur_token.token_type == TokenType::Rparem {
            self.next_token();
        } else {
            return Err(Error { errors: vec![format!("[parse_if_statement] expected next token to be Rparem, got {:?}", self.cur_token.token_type)] });
        }
        let consequence = self.parse_statement()?;
        if self.peek_token.token_type != TokenType::Else {
            return Ok(Statement::If { condition, consequence: Box::new(consequence), alternative: None });
        } else {
            self.next_token(); // read `}` or `;`
            self.next_token(); // read `else`
            let alternative = self.parse_statement()?;
            return Ok(Statement::If { condition, consequence: Box::new(consequence), alternative: Some(Box::new(alternative)) });
        }
    }

    // switch (<expression>) { case <value>: <statement>* case <value>: <statement>* ... default: <statement>* }
    fn parse_switch_statement(&mut self) -> Result<Statement> {
        self.next_token();
        if self.cur_token.token_type != TokenType::Lparem {
            return Err(Error { errors: vec![format!("[parse_switch_statement] expected next token to be Lparem, got {:?}", self.cur_token.token_type)] });
        }

        self.next_token();
        let condition = self.parse_expression(ExpressionPrecedence::Lowest)?;

        self.next_token();
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_switch_statement] expected next token to be Rparem, got {:?}", self.cur_token.token_type)] });
        }

        self.next_token();
        if self.cur_token.token_type != TokenType::Lbrace {
            return Err(Error { errors: vec![format!("[parse_switch_statement] expected next token to be Lbrace, got {:?}", self.cur_token.token_type)] });
        }

        self.next_token();
        let switch_body = self.parse_switch_body()?;
        return Ok(Statement::Switch {
            condition,
            switch_block: switch_body,
        });
    }

    fn parse_switch_body(&mut self) -> Result<SwitchBlock> {
        let mut body: Vec<Statement> = vec![];
        let mut body_index = 0;
        let mut switch_label_entry: Vec<SwitchLabelEntry> = vec![];
        loop {
            if self.cur_token.token_type != TokenType::Case && self.cur_token.token_type != TokenType::Default {
                return Err(Error { errors: vec![format!("[parse_switch_body] expected next token to be Case or Default, got {:?}", self.cur_token.token_type)] });
            }
            let mut labels: Vec<SwitchLabel> = vec![];

            // `case <value>:` が連続することがあるのでループで処理する
            while self.cur_token.token_type == TokenType::Case {
                self.next_token();
                let label = self.parse_expression(ExpressionPrecedence::Lowest)?;
                self.next_token();
                if self.cur_token.token_type == TokenType::Colon {
                    self.next_token();
                } else {
                    return Err(Error { errors: vec![format!("[parse_switch_body] expected next token to be Colon, got {:?}", self.cur_token.token_type)] });
                }
                labels.push(SwitchLabel::Case(label));
            }

            // `default:` を処理する
            if self.cur_token.token_type == TokenType::Default {
                self.next_token();
                if self.cur_token.token_type == TokenType::Colon {
                    self.next_token();
                } else {
                    return Err(Error { errors: vec![format!("[parse_switch_body] expected next token to be Colon, got {:?}", self.cur_token.token_type)] });
                }
                labels.push(SwitchLabel::Default);
            }

            let label_entry = SwitchLabelEntry {
                labels,
                start_index: body_index,
            };
            switch_label_entry.push(label_entry);

            // ブロックを処理する
            while self.cur_token.token_type != TokenType::Case && self.cur_token.token_type != TokenType::Default && self.cur_token.token_type != TokenType::Rbrace {
                if self.cur_token.token_type == TokenType::Eof {
                    return Err(Error { errors: vec!["[parse_switch_body] unexpected end of file".to_string()] });
                }
                let stmt = self.parse_statement()?;
                body.push(stmt);
                body_index += 1;
                self.next_token();
            }

            if self.cur_token.token_type == TokenType::Rbrace {
                break;
            }
        }
        return Ok(SwitchBlock { label_entries: switch_label_entry, body, });
    }

    // break;
    fn parse_break_statement(&mut self) -> Result<Statement> {
        self.next_token();
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(Statement::Break);
        } else {
            return Err(Error { errors: vec![format!("[parse_break_statement] expected next token to be SEMICOLON, got {:?}", self.cur_token.token_type)] });
        }
    }

    // while (<expression>) <block_statement> | <expression_statement>
    fn parse_while_statement(&mut self) -> Result<Statement> {
        self.next_token();
        if self.cur_token.token_type != TokenType::Lparem {
            return Err(Error { errors: vec![format!("[parse_while_statement] expected next token to be Lparem, got {:?}", self.cur_token.token_type)] });
        }

        self.next_token();
        let condition = self.parse_expression(ExpressionPrecedence::Lowest)?;

        self.next_token();
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_while_statement] expected next token to be Rparem, got {:?}", self.cur_token.token_type)] });
        }

        self.next_token();
        let body = self.parse_statement()?;

        if self.cur_token.token_type != TokenType::Rbrace && self.cur_token.token_type != TokenType::Semicolon {
            return Err(Error { errors: vec![format!("[parse_while_statement] expected next token to be RBrace or Semicolon, got {:?}", self.cur_token.token_type)] });
        }

        return Ok(Statement::While {
            condition,
            body: Box::new(body),
        });
    }

    // do <block_statement> while (<expression>) | do <expression_statement> while (<expression>);
    fn parse_dowhile_statement(&mut self) -> Result<Statement> {
        self.next_token();
        let body = self.parse_statement()?;

        self.next_token();
        if self.cur_token.token_type != TokenType::While {
            return Err(Error { errors: vec![format!("[parse_dowhile_statement] expected next token to be While, got {:?}", self.cur_token.token_type)] });
        }

        self.next_token();
        if self.cur_token.token_type != TokenType::Lparem {
            return Err(Error { errors: vec![format!("[parse_dowhile_statement] expected next token to be Lparem, got {:?}", self.cur_token.token_type)] });
        }

        self.next_token();
        let condition = self.parse_expression(ExpressionPrecedence::Lowest)?;

        self.next_token();
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_dowhile_statement] expected next token to be Rparem, got {:?}", self.cur_token.token_type)] });
        }

        self.next_token();
        if self.cur_token.token_type != TokenType::Semicolon {
            return Err(Error { errors: vec![format!("[parse_dowhile_statement] expected next token to be Semicolon, got {:?}", self.cur_token.token_type)] });
        }

        return Ok(Statement::DoWhile {
            body: Box::new(body),
            condition,
        });
    }

    // for (<expression>; <expression>; <expression>) <block_statement> | <expression_statement>
    fn parse_for_statement(&mut self) -> Result<Statement> {
        self.next_token();
        if self.cur_token.token_type != TokenType::Lparem {
            return Err(Error { errors: vec![format!("[parse_for_statement] expected next token to be Lparem, got {:?}", self.cur_token.token_type)] });
        }

        self.next_token();
        let init =
            if self.cur_token.token_type == TokenType::Semicolon {
                None
            } else {
                let some = Some(self.parse_expression(ExpressionPrecedence::Lowest)?);
                self.next_token();
                if self.cur_token.token_type != TokenType::Semicolon {
                    return Err(Error { errors: vec![format!("[parse_for_statement -> init] expected next token to be Semicolon, got {:?}", self.cur_token.token_type)] });
                }
                some
            };
        self.next_token();
        let condition = 
            if self.cur_token.token_type == TokenType::Semicolon {
                None
            } else {
                let some = Some(self.parse_expression(ExpressionPrecedence::Lowest)?);
                self.next_token();
                if self.cur_token.token_type != TokenType::Semicolon {
                    return Err(Error { errors: vec![format!("[parse_for_statement -> condition] expected next token to be Semicolon, got {:?}", self.cur_token.token_type)] });
                }
                some
            };
        self.next_token();
        let post = 
            if self.cur_token.token_type == TokenType::Rparem {
                None
            } else {
                let some = Some(self.parse_expression(ExpressionPrecedence::Lowest)?);
                self.next_token();
                if self.cur_token.token_type != TokenType::Rparem {
                    return Err(Error { errors: vec![format!("[parse_for_statement -> post] expected next token to be Rparem, got {:?}", self.cur_token.token_type)] });
                }
                some
            };

        self.next_token();
        let body = self.parse_statement()?;

        if self.cur_token.token_type != TokenType::Rbrace && self.cur_token.token_type != TokenType::Semicolon {
            return Err(Error { errors: vec![format!("[parse_while_statement] expected next token to be RBrace or Semicolon, got {:?}", self.cur_token.token_type)] });
        }

        return Ok(Statement::For {
            init,
            condition,
            post,
            body: Box::new(body),
        });
    }

    // continue;
    fn parse_continue_statement(&mut self) -> Result<Statement> {
        self.next_token();
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(Statement::Continue);
        } else {
            return Err(Error { errors: vec![format!("expected next token to be SEMICOLON, got {:?}", self.cur_token.token_type)] });
        }
    }

    // <expression>;
    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expression = self.parse_expression(ExpressionPrecedence::Lowest);
        let result = expression.map(|exp| Statement::ExpressionStatement(exp))?;
        self.next_token();
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(result);
        } else {
            let error_msg = format!("[parse_expression_statement] expected next token to be SEMICOLON, got {:?}", self.cur_token.token_type);
            return Err(Error { errors: vec![error_msg] });
        }
    }

}

#[cfg(test)]
mod tests {

    use crate::{lexer::Lexer, parser::ast::Expression};
    use super::*;

    #[test]
    fn test_vardecl() {
        // given
        let input = "
int five = 5;
int x = 10;
int x;
int x, y, z;
char c = 'a';
struct { int x; int y; } p;
struct { int a; int b; } p, q;
struct point z;
struct { int x; int y; } p = { 10, 20 };
struct User a = { 1, 2 };
";
        let expected = vec![
            vec![("int", "five", Some("5"))],
            vec![("int", "x", Some("10"))],
            vec![("int", "x", None)],
            vec![("int", "x", None), ("int", "y", None), ("int", "z", None)],
            vec![("char", "c", Some("a"))],
            vec![("struct {\n    int x;\n    int y;\n}", "p", None)],
            vec![("struct {\n    int a;\n    int b;\n}", "p", None), ("struct {\n    int a;\n    int b;\n}", "q", None)],
            vec![("struct point", "z", None)],
            vec![("struct {\n    int x;\n    int y;\n}", "p", Some("{10, 20}"))],
            vec![("struct User", "a", Some("{1, 2}"))],
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::VarDecl(declarators) => {
                    for (i, (type_dec, declarator)) in declarators.iter().enumerate() {
                        let (expected_type, expected_name, expected_value) = &expected[row_num][i];
                        assert_eq!(type_dec.type_name(), expected_type.to_string());
                        assert_eq!(declarator.name, *expected_name);
                        assert_eq!(declarator.value.as_ref().map(|x| x.to_string()), expected_value.map(|x| x.to_string()));
                    }
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
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::Return(value) => {
                    let expected_value= expected[row_num];
                    assert_eq!(value, &Some(Expression::Int(expected_value)));
                }
                _ => panic!("Statement is not Return"),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        // given
        let input = "
x;
foobar;
";
        let expected = vec!["x", "foobar"];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::ExpressionStatement(expression) => {
                    let expected_value= expected[row_num];
                    assert_eq!(expression, &Expression::Identifier(expected_value.to_string()));
                }
                _ => panic!("Statement is not ExpressionStatement"),
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
            ("!", Expression::Identifier("foobar".to_string())),
            ("-", Expression::Int(5)),
            ("++", Expression::Identifier("a".to_string())),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::ExpressionStatement(Expression::PrefixExpression { operator, right }) => {
                    let (expected_operator, expected_right)= &expected[row_num];
                    assert_eq!(operator, expected_operator);
                    assert_eq!(right.as_ref(), expected_right);
                }
                _ => panic!("Statement is not ExpressionStatement"),
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
5 % 6;
5 == 6;
5 != 6;
5 > 6;
5 < 6;
xyz = 10;
a += 3;
a -= 3;
a *= 3;
a /= 3;
a %= 3;
";
        let expected = vec![
            ("+", Expression::Int(5), Expression::Int(6)),
            ("-", Expression::Int(5), Expression::Int(6)),
            ("*", Expression::Int(5), Expression::Int(6)),
            ("/", Expression::Int(5), Expression::Int(6)),
            ("%", Expression::Int(5), Expression::Int(6)),
            ("==", Expression::Int(5), Expression::Int(6)),
            ("!=", Expression::Int(5), Expression::Int(6)),
            (">", Expression::Int(5), Expression::Int(6)),
            ("<", Expression::Int(5), Expression::Int(6)),
            ("=", Expression::Identifier("xyz".to_string()), Expression::Int(10)),
            ("+=", Expression::Identifier("a".to_string()), Expression::Int(3)),
            ("-=", Expression::Identifier("a".to_string()), Expression::Int(3)),
            ("*=", Expression::Identifier("a".to_string()), Expression::Int(3)),
            ("/=", Expression::Identifier("a".to_string()), Expression::Int(3)),
            ("%=", Expression::Identifier("a".to_string()), Expression::Int(3)),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::ExpressionStatement(Expression::InfixExpression { operator, left, right }) => {
                    let (expected_operator, expected_left, expected_right)= &expected[row_num];
                    assert_eq!(operator, expected_operator);
                    assert_eq!(left.as_ref(), expected_left);
                    assert_eq!(right.as_ref(), expected_right);
                }
                _ => panic!("Statement is not ExpressionStatement"),
            }
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
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::If { condition, consequence, alternative } => {
                    let (expected_condition, expected_consequence, expected_alternative) = expected[row_num];
                    assert_eq!(condition.to_string(), expected_condition.to_string());
                    assert_eq!(consequence.to_string(), expected_consequence.to_string());
                    assert_eq!(alternative.as_ref().map(|a| a.to_string()), expected_alternative.map(|a| a.to_string()));
                }
                _ => panic!("Statement is not If"),
            }
        }
    }

    #[test]
    fn test_switch_statement() {
        // given
        let input = "
switch (a) {
    case 1:
        x;
        break;
    case 2:
        y;
        break;
    case 3:
        aaa;
        break;
    default:
        bbb;
}

switch (x) {
case 1:
case 2:
    bbb;
case 3:
    ccc;
    break;
}
";
        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        let rows_count = 2;
        for _ in 0..rows_count {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), rows_count);
        let stmt1 = parse_results.first().unwrap();
        let stmt2 = parse_results.get(1).unwrap();

        // 1つ目のswitch文のチェック
        if let Statement::Switch { condition, switch_block } = stmt1 {
            assert_eq!(condition.to_string(), "a");
            // 各ラベルのチェック
            assert_eq!(switch_block.label_entries.len(), 4);
            assert_eq!(switch_block.label_entries[0].labels[0], SwitchLabel::Case(Expression::Int(1)));
            assert_eq!(switch_block.label_entries[0].start_index, 0);
            assert_eq!(switch_block.label_entries[1].labels[0], SwitchLabel::Case(Expression::Int(2)));
            assert_eq!(switch_block.label_entries[1].start_index, 2);
            assert_eq!(switch_block.label_entries[2].labels[0], SwitchLabel::Case(Expression::Int(3)));
            assert_eq!(switch_block.label_entries[2].start_index, 4);
            assert_eq!(switch_block.label_entries[3].labels[0], SwitchLabel::Default);
            assert_eq!(switch_block.label_entries[3].start_index, 6);

            assert_eq!(switch_block.body.len(), 7);
            if let Statement::ExpressionStatement(expression) = &switch_block.body[0] {
                assert_eq!(expression, &Expression::Identifier("x".to_string()));
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
            match &switch_block.body[1] {
                Statement::Break => {}
                _ => panic!("Expected Statement with Break"),
            }
            if let Statement::ExpressionStatement(expression) = &switch_block.body[2] {
                assert_eq!(expression, &Expression::Identifier("y".to_string()));
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
            match &switch_block.body[3] {
                Statement::Break => {}
                _ => panic!("Expected Statement with Break"),
            }
            if let Statement::ExpressionStatement(expression) = &switch_block.body[4] {
                assert_eq!(expression, &Expression::Identifier("aaa".to_string()));
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
            match &switch_block.body[5] {
                Statement::Break => {}
                _ => panic!("Expected Statement with Break"),
            }
            if let Statement::ExpressionStatement(expression) = &switch_block.body[6] {
                assert_eq!(expression, &Expression::Identifier("bbb".to_string()));
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
        } else {
            panic!("Statement is not Switch");
        }

        // 2つ目のswitch文のチェック
        if let Statement::Switch { condition, switch_block } = stmt2 {
            assert_eq!(condition.to_string(), "x");
            // 各ラベルのチェック
            assert_eq!(switch_block.label_entries.len(), 2);
            assert_eq!(switch_block.label_entries[0].labels[0], SwitchLabel::Case(Expression::Int(1)));
            assert_eq!(switch_block.label_entries[0].labels[1], SwitchLabel::Case(Expression::Int(2)));
            assert_eq!(switch_block.label_entries[0].start_index, 0);
            assert_eq!(switch_block.label_entries[1].labels[0], SwitchLabel::Case(Expression::Int(3)));
            assert_eq!(switch_block.label_entries[1].start_index, 1);

            assert_eq!(switch_block.body.len(), 3);
            if let Statement::ExpressionStatement(expression) = &switch_block.body[0] {
                assert_eq!(expression, &Expression::Identifier("bbb".to_string()));
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
            if let Statement::ExpressionStatement(expression) = &switch_block.body[1] {
                assert_eq!(expression, &Expression::Identifier("ccc".to_string()));
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
            match &switch_block.body[2] {
                Statement::Break => {}
                _ => panic!("Expected Statement with Break"),
            }
        } else {
            panic!("Statement is not Switch");
        }
    }

    #[test]
    fn test_while_statement() {
        // given
        let input = "
while (x > y) { x; y; }
while (x < y) x + 2;
while (x < y) {
    if (x == 0) {
        break;
    }
    if (y > 10) {
        continue;
    }
}
";

        let expected = vec![
            ("(x > y)", "{\n    x;\n    y;\n}"),
            ("(x < y)", "(x + 2);"),
            ("(x < y)", "{\n    if ((x == 0)) {\n    break;\n}\n    if ((y > 10)) {\n    continue;\n}\n}"),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::While { condition, body} => {
                    let (expected_condition, expected_body) = expected[row_num];
                    assert_eq!(condition.to_string(), expected_condition.to_string());
                    assert_eq!(body.to_string(), expected_body.to_string());
                }
                _ => panic!("Statement is not While"),
            }
        }
    }

    #[test]
    fn test_dowhile_statement() {
        // given
        let input = "
do { x; y; } while (x > y);
do x + 2; while (x < y);
";

        let expected = vec![
            ("{\n    x;\n    y;\n}", "(x > y)"),
            ("(x + 2);", "(x < y)"),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::DoWhile { body, condition } => {
                    let (expected_body, expected_condition) = expected[row_num];
                    assert_eq!(body.to_string(), expected_body.to_string());
                    assert_eq!(condition.to_string(), expected_condition.to_string());
                }
                _ => panic!("Statement is not DoWhile"),
            }
        }
    }

    #[test]
    fn test_for_statement() {
        // given
        let input = "
for (i = 0; i < len; i++) { x; }
for (;x < y;) x + 2;
for (;;) ++a;
";

        let expected = vec![
            ("(i = 0)", "(i < len)", "(i++)", "{\n    x;\n}"),
            ("", "(x < y)", "", "(x + 2);"),
            ("", "", "", "(++a);"),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::For { init, condition, post, body} => {
                    let (expected_init, expected_condition, expected_post, expected_body) = expected[row_num];
                    assert_eq!(init.as_ref().map(|x| x.to_string()).unwrap_or("".to_string()), expected_init.to_string());
                    assert_eq!(condition.as_ref().map(|x| x.to_string()).unwrap_or_default(), expected_condition.to_string());
                    assert_eq!(post.as_ref().map(|x| x.to_string()).unwrap_or_default(), expected_post.to_string());
                    assert_eq!(body.to_string(), expected_body.to_string());
                }
                _ => panic!("Statement is not For"),
            }
        }
    }

    #[test]
    fn test_vardecl_pointer_array() {
        // given
        let input = "
int *five;
int* a;
int **b;
int a[10];
int b[3][5];
int c[] = { 1, 2 };
int d[][4] = { {1, 2, 3, 4}, {1, 2, 3, 4} };
int e[] = {};
int f[] = {1, 2, };
char *s = \"Hello!\\n\";
struct key {
    char *word;
    int count;
} keytab[3];
struct key keytab[3];
";
        let expected = vec![
            ("int*", "five", None),
            ("int*", "a", None),
            ("int**", "b", None),
            ("int[10]", "a", None),
            ("int[3][5]", "b", None),
            ("int[]", "c", Some("{1, 2}")),
            ("int[][4]", "d", Some("{{1, 2, 3, 4}, {1, 2, 3, 4}}")),
            ("int[]", "e", Some("{}")),
            ("int[]", "f", Some("{1, 2}")),
            ("char*", "s", Some("Hello!\\n")),
            ("struct key {\n    char* word;\n    int count;\n}[3]", "keytab", None),
            ("struct key[3]", "keytab", None),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::VarDecl(declarators) => {
                    let (type_dec, declarator) = declarators.first().unwrap();
                    let (expected_type, expected_name, expected_value) = &expected[row_num];
                    assert_eq!(type_dec.type_name(), expected_type.to_string());
                    assert_eq!(declarator.name, *expected_name);
                    assert_eq!(declarator.value.as_ref().map(|x| x.to_string()), expected_value.map(|x|x.to_string()));
                }
                _ => panic!("Statement is not VarDecl"),
            }
        }
    }

    #[test]
    fn test_function_call_expression() {
        // given
        let input = "
foo();
bar(a, b);
piyo(3+2, b);
";
        let expected = vec![
            ("foo", vec![]),
            ("bar", vec!["a", "b"]),
            ("piyo", vec!["(3 + 2)", "b"]),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (i, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::ExpressionStatement(Expression::FunctionCallExpression { function_name, arguments }) => {
                    let (expected_function_name, expected_arguments)= &expected[i];
                    assert_eq!(function_name, expected_function_name);
                    let xs: Vec<String> = arguments.iter().map(|x| x.to_string()).collect();
                    assert_eq!(xs, *expected_arguments);
                }
                _ => panic!("Statement is not Expression::FunctionCallExpression"),
            }
        }
    }

    #[test]
    fn test_postfix_expression() {
        // given
        let input = "
i++;
a--;
";
        let expected = vec![
            ("++", Expression::Identifier("i".to_string())),
            ("--", Expression::Identifier("a".to_string())),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<Statement> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, stmt) in parse_results.iter().enumerate() {
            match stmt {
                Statement::ExpressionStatement(Expression::PostfixExpression { operator, left }) => {
                    let (expected_operator, expected_right)= &expected[row_num];
                    assert_eq!(operator, expected_operator);
                    assert_eq!(left.as_ref(), expected_right);
                }
                _ => panic!("Statement is not PostfixExpression"),
            }
        }
    }
}
