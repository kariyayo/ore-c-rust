use std::collections::HashMap;
use crate::{lexer::{self, token::TokenType}, parser::ast::TypeRef};

mod ast;
use ast::{Declarator, Expression, Statement, SwitchBlock, SwitchLabel, SwitchLabelEntry};

#[derive(Debug)]
struct Error {
    errors: Vec<String>,
}

type Result<T> = std::result::Result<T, Error>;

/// Pratt構文解析器
/// 
/// 式の解析において、構文解析関数を文法ルールに関連づけるのではなく、単一のトークンタイプに関連づける。
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

// 式の優先順位。
// 値が大きいほど優先順位が高く、ASTの深いレベルに配置される。
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum ExpressionPrecedence {
    Lowest,
    Assign, // =
    Equals, // ==
    LessGreater, // > または <
    Sum, // +
    Product, // *
    Prefix, // -X, !X, ++X, --X
    Postfix, // X++, X--
    Call, // myFunction(X)
    Index,
}

fn is_infix_token_type(token_type: TokenType) -> bool {
    return match token_type {
        TokenType::Plus
        | TokenType::Minus
        | TokenType::Slash
        | TokenType::Asterisk
        | TokenType::Percent
        | TokenType::Eq
        | TokenType::NotEq
        | TokenType::Lt
        | TokenType::Gt
        | TokenType::Assign
        | TokenType::PlusAssign
        | TokenType::MinusAssign
        | TokenType::AsteriskAssign
        | TokenType::SlashAssign
        | TokenType::PercentAssign
        | TokenType::Lparem
        | TokenType::Lbracket => {
            true
        }
        _ => {
            false
        }
    };
}

fn is_postfix_token_type(token_type: TokenType) -> bool {
    return match token_type {
        TokenType::Increment | TokenType::Decrement => {
            true
        }
        _ => false
    }
}

impl Parser {
    pub(crate) fn new(l: lexer::Lexer) -> Parser {
        let mut precedences: HashMap<TokenType, ExpressionPrecedence> = HashMap::with_capacity(8);
        precedences.insert(TokenType::Assign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::PlusAssign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::MinusAssign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::AsteriskAssign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::SlashAssign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::PercentAssign, ExpressionPrecedence::Assign);
        precedences.insert(TokenType::Eq, ExpressionPrecedence::Equals);
        precedences.insert(TokenType::NotEq, ExpressionPrecedence::Equals);
        precedences.insert(TokenType::Lt, ExpressionPrecedence::LessGreater);
        precedences.insert(TokenType::Gt, ExpressionPrecedence::LessGreater);
        precedences.insert(TokenType::Plus, ExpressionPrecedence::Sum);
        precedences.insert(TokenType::Minus, ExpressionPrecedence::Sum);
        precedences.insert(TokenType::Slash, ExpressionPrecedence::Product);
        precedences.insert(TokenType::Asterisk, ExpressionPrecedence::Product);
        precedences.insert(TokenType::Percent, ExpressionPrecedence::Product);
        precedences.insert(TokenType::Increment, ExpressionPrecedence::Postfix);
        precedences.insert(TokenType::Decrement, ExpressionPrecedence::Postfix);
        precedences.insert(TokenType::Lparem, ExpressionPrecedence::Call);
        precedences.insert(TokenType::Lbracket, ExpressionPrecedence::Index);
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
        let mut program = ast::Program { external_items: vec![] };
        while self.cur_token.token_type != TokenType::Eof {
            let external_item = self.parse_external_item();
            match external_item {
                Ok(item) => {
                    program.external_items.push(item);
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

    fn parse_external_item(&mut self) -> Result<ast::ExternalItem> {
        match self.cur_token.token_type {
            TokenType::Int | TokenType::Void | TokenType::Char | TokenType::Short | TokenType::Long | TokenType::Ident => {
                // 関数宣言で、返り値の型を省略している場合を考慮する
                let type_dec = if self.peek_token.token_type == TokenType::Lparem {
                    // 省略されている場合は、int型とする
                    ast::TypeRef::Named("int".to_string())
                } else {
                    let tmp = ast::TypeRef::Named(self.cur_token.literal.to_string());
                    self.next_token();
                    tmp
                };

                if self.peek_token.token_type == TokenType::Lparem {
                    // 関数
                    return self.parse_external_function(type_dec);
                } else {
                    // 変数
                    return self.parse_external_vardecl(type_dec);
                }
            }
            _ => {
                return Err(Error { errors: vec![format!("[parse_external_item] expected external item token, got {:?}", self.cur_token.token_type)] });
            }
        }
    }

    fn parse_external_vardecl(&mut self, type_dec: ast::TypeRef) -> Result<ast::ExternalItem> {
        let declarators = self.parse_declarators(type_dec)?;
        if self.cur_token.token_type != TokenType::Semicolon {
            return Err(Error { errors: vec![format!("[parse_external_item] expected next token to be Semicolon, got {:?}", self.peek_token.token_type)] });
        }
        return Ok(ast::ExternalItem::VarDecl { declarators });
    }

    fn parse_external_function(&mut self, return_type_dec: ast::TypeRef) -> Result<ast::ExternalItem> {
        if self.cur_token.token_type != TokenType::Ident {
            return Err(Error { errors: vec![format!("[parse_external_item] expected next token to be IDENT, got {:?}", self.peek_token.token_type)] });
        }
        let name = self.cur_token.literal.clone();

        self.next_token(); // cur_token is Lparam
        let parameters = self.parse_function_params()?;
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_external_item] expected next token to be Rparem, got {:?}", self.peek_token.token_type)] });
        }
        self.next_token();
        return if self.cur_token.token_type == TokenType::Lbrace {
            let body = self.parse_block_statement()?;
            Ok(ast::ExternalItem::FunctionDecl { return_type_dec, name, parameters, body: Some(Box::new(body)) })
        } else {
            Ok(ast::ExternalItem::FunctionDecl { return_type_dec, name, parameters, body: None })
        };
    }

    fn parse_function_params(&mut self) -> Result<Vec<ast::Parameter>> {
        let mut parameters: Vec<ast::Parameter> = vec![];
        if self.cur_token.token_type != TokenType::Lparem {
            return Err(Error { errors: vec![format!("[parse_function_params] expected next token to be Lparem, got {:?}", self.peek_token.token_type)] });
        }
        self.next_token();
        if self.cur_token.token_type == TokenType::Rparem {
            return Ok(parameters);
        }
        loop {
            let mut type_dec = ast::TypeRef::Named(self.cur_token.literal.clone());
            self.next_token();

            // pointer?
            while self.cur_token.token_type == TokenType::Asterisk {
                type_dec = ast::TypeRef::Pointer(Box::new(type_dec));
                self.next_token();
            }

            if self.cur_token.token_type != TokenType::Ident {
                return Err(Error { errors: vec![format!("[parse_function_params] expected next token to be IDENT, got {:?}", self.cur_token.token_type)] });
            }
            let name = self.cur_token.literal.clone();

            // array?
            while self.peek_token.token_type == TokenType::Lbracket {
                self.next_token(); // cur_token is `[`
                self.next_token();
                if self.cur_token.token_type != TokenType::Rbracket {
                    return Err(Error { errors: vec![format!("[parse_function_params] expected next token to be Rbracket, got {:?}", self.cur_token.token_type)] });
                }
                type_dec = ast::TypeRef::Array(Box::new(type_dec), None);
            }

            let parameter = ast::Parameter { type_dec, name };
            parameters.push(parameter);

            self.next_token();
            if self.cur_token.token_type != TokenType::Comma {
                break;
            }
            self.next_token(); // read `,`
        }
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_function_params] expected next token to be Rparem, got {:?}", self.cur_token.token_type)] });
        }
        return Ok(parameters);
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
    // <type_ref> <ident>, <ident>, ...;
    // <type_ref> * <ident>;
    // <type_ref> <ident>[<size>];
    // <type_ref> <ident>[] = {<expression>, <expression>, ...};
    //
    // ex) int a, *b, c[10];
    fn parse_vardecl_statement(&mut self) -> Result<ast::Statement> {
        let type_dec = ast::TypeRef::Named(self.cur_token.literal.to_string());
        self.next_token();
        let declarators: Vec<(TypeRef, Declarator)>= self.parse_declarators(type_dec)?;
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(ast::Statement::VarDecl { declarators });
        } else {
            let error_msg = format!("[parse_vardecl_statement] expected next token to be SEMICOLON, got {:?}", self.cur_token.token_type);
            return Err(Error { errors: vec![error_msg] });
        }
    }

    fn parse_declarators(&mut self, base_type_dec: TypeRef) -> Result<Vec<(TypeRef, Declarator)>> {
        let base_type_name = match base_type_dec {
            TypeRef::Named(name) => name,
            _ => {
                return Err(Error { errors: vec![format!("[parse_declarators] expected `base_type_dec` should be TypeRef::Named, got {:?}", base_type_dec)] });
            }
        };
        let mut result: Vec<(TypeRef, Declarator)>= vec![];
        loop {
            let mut type_dec = TypeRef::Named(base_type_name.clone());

            // pointer?
            while self.cur_token.token_type == TokenType::Asterisk {
                type_dec = ast::TypeRef::Pointer(Box::new(type_dec));
                self.next_token();
            }

            if self.cur_token.token_type != TokenType::Ident {
                return Err(Error { errors: vec![format!("[parse_declarators] expected next token to be IDENT, got {:?}", self.cur_token.token_type)] });
            }
            let name = self.cur_token.literal.clone();

            // array?
            while self.peek_token.token_type == TokenType::Lbracket {
                self.next_token(); // cur_token is `[`
                self.next_token();
                if self.cur_token.token_type == TokenType::Rbracket {
                    // <type_ref> <ident>[] = {<expression>, <expression>, ...};
                    type_dec = ast::TypeRef::Array(Box::new(type_dec), None);
                } else {
                    // <type_ref> <ident>[<size>];
                    if self.cur_token.token_type != TokenType::Integer {
                        return Err(Error { errors: vec![format!("[parse_declarators] expected next token to be Integer, got {:?}", self.cur_token.token_type)] });
                    }
                    let size = self.cur_token.literal
                        .parse::<u32>()
                        .map_err(|_| Error { errors: vec![format!("[parse_declarators] failed to parse integer size from {:?}", self.cur_token.literal)] })?;
                    type_dec = ast::TypeRef::Array(Box::new(type_dec), Some(size));
                    self.next_token();
                    if self.cur_token.token_type != TokenType::Rbracket {
                        return Err(Error { errors: vec![format!("[parse_declarators] expected next token to be RBracket, got {:?}", self.cur_token.token_type)] });
                    }
                }
            }

            let declarator = if self.peek_token.token_type != TokenType::Assign {
                // 値の指定がない、かつ、サイズが指定されていない配列型の場合はエラー
                if let ast::TypeRef::Array(_, None) = type_dec {
                    return Err(Error { errors: vec![format!("[parse_declarators] expected next token to be Assign, got {:?}", self.peek_token.token_type)] });
                }
                ast::Declarator { name, value: None }
            } else {
                self.next_token(); // cur_token is `=`
                self.next_token(); // `=` を読み飛ばす
                let value = self.parse_expression(ExpressionPrecedence::Lowest)?;
                ast::Declarator { name, value: Some(value) }
            };

            result.push((type_dec, declarator));
            self.next_token();
            if self.cur_token.token_type != TokenType::Comma {
                break;
            }
            self.next_token(); // `,` を読み飛ばす
        }
        return Ok(result);
    }

    // { <statement>* }
    fn parse_block_statement(&mut self) -> Result<ast::Statement> {
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
            return Ok(ast::Statement::Block { statements });
        } else if self.cur_token.token_type == TokenType::Rbrace {
            return Ok(ast::Statement::Block { statements });
        } else {
            let error_msg = format!("[parse_block_statement] expected next token to be Rbrace, got {:?}", self.cur_token.token_type);
            return Err(Error { errors: vec![error_msg] });
        }
    }

    // if (<expression>) <block_statement> | <expression_statement>
    // if (<expression>) <block_statement> | <expression_statement> else <block_statement> | <expression_statement>
    fn parse_if_statement(&mut self) -> Result<ast::Statement> {
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
            return Ok(ast::Statement::If { condition, consequence: Box::new(consequence), alternative: None });
        } else {
            self.next_token(); // read `}` or `;`
            self.next_token(); // read `else`
            let alternative = self.parse_statement()?;
            return Ok(ast::Statement::If { condition, consequence: Box::new(consequence), alternative: Some(Box::new(alternative)) });
        }
    }

    // switch (<expression>) { case <value>: <statement>* case <value>: <statement>* ... default: <statement>* }
    fn parse_switch_statement(&mut self) -> Result<ast::Statement> {
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
        return Ok(ast::Statement::Switch {
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

            let label_entry = ast::SwitchLabelEntry {
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
    fn parse_break_statement(&mut self) -> Result<ast::Statement> {
        self.next_token();
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(ast::Statement::Break);
        } else {
            return Err(Error { errors: vec![format!("[parse_break_statement] expected next token to be SEMICOLON, got {:?}", self.cur_token.token_type)] });
        }
    }

    // while (<expression>) <block_statement> | <expression_statement>
    fn parse_while_statement(&mut self) -> Result<ast::Statement> {
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

        return Ok(ast::Statement::While {
            condition,
            body: Box::new(body),
        });
    }

    // do <block_statement> while (<expression>) | do <expression_statement> while (<expression>);
    fn parse_dowhile_statement(&mut self) -> Result<ast::Statement> {
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

        return Ok(ast::Statement::DoWhile {
            body: Box::new(body),
            condition,
        });
    }

    // for (<expression>; <expression>; <expression>) <block_statement> | <expression_statement>
    fn parse_for_statement(&mut self) -> Result<ast::Statement> {
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

        return Ok(ast::Statement::For {
            init,
            condition,
            post,
            body: Box::new(body),
        });
    }

    // continue;
    fn parse_continue_statement(&mut self) -> Result<ast::Statement> {
        self.next_token();
        if self.cur_token.token_type == TokenType::Semicolon {
            return Ok(ast::Statement::Continue);
        } else {
            return Err(Error { errors: vec![format!("expected next token to be SEMICOLON, got {:?}", self.cur_token.token_type)] });
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
                return Err(Error { errors: vec![format!("[parse_expression] no prefix parse function for {:?}", self.cur_token.token_type)] });
            }
            Some(Err(e)) => {
                return Err(e);
            }
            Some(Ok(exp)) => {
                let mut result = exp;
                loop {
                    // 次のトークンがセミコロンの場合は文が終了するので、ここでparse結果を返す。
                    //
                    // もしくは、
                    //
                    // 現在の演算子の優先順位(precedence)が、次のトークンの優先順位 以上 である場合は、
                    // ここでparse結果を返す。
                    // 結果的に、優先順位が高いほどASTの深いレベルに配置されることになる。
                    //
                    // 例えば、`5 * 5 + 3` の場合、`5 * 5` が終わった時点で `*` の優先順位(`precedence`)が
                    // 次の演算子 `+` の優先順位(`self.peek_precedence`)より高いので `5 * 5` のparse結果を返す。
                    if self.peek_token.token_type == TokenType::Semicolon || precedence >= self.peek_precedence() {
                        return Ok(result);
                    }

                    if is_infix_token_type(self.peek_token.token_type) {
                        let left_exp = result;
                        self.next_token();
                        let infix_result = self.infix(left_exp);
                        result = infix_result?;
                    } else if is_postfix_token_type(self.peek_token.token_type) {
                        let left_exp = result;
                        self.next_token();
                        let postfix_result = self.parse_postfix_expression(left_exp);
                        result = postfix_result;
                    } else {
                        return Ok(result);
                    }
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
            TokenType::Bang
            | TokenType::Minus
            | TokenType::Increment
            | TokenType::Decrement
            | TokenType::Asterisk
            | TokenType::Ampersand => {
                Some(self.parse_prefix_expression())
            }
            TokenType::Lparem => {
                Some(self.parse_grouped_expression())
            }
            TokenType::Lbrace => {
                Some(self.parse_array_initializer_expression())
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
            .map_err(|_| Error { errors: vec!["[parse_integer_literal] parse int error".to_string()] });
    }

    fn parse_grouped_expression(&mut self) -> Result<ast::Expression> {
        self.next_token();
        let exp = self.parse_expression(ExpressionPrecedence::Lowest);
        if self.peek_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_grouped_expression] expected next token to be Rparem, got {:?}", self.peek_token.token_type)] });
        }
        self.next_token();
        return exp;
    }

    fn parse_array_initializer_expression(&mut self) -> Result<ast::Expression> {
        self.next_token();
        let mut elements: Vec<ast::Expression> = vec![];
        if self.cur_token.token_type == TokenType::Rbrace {
            return Ok(ast::Expression::ArrayInitializerExpression { elements });
        }
        while self.cur_token.token_type != TokenType::Rbrace {
            let value = self.parse_expression(ExpressionPrecedence::Lowest)?;
            elements.push(value);
            self.next_token();
            if self.cur_token.token_type != TokenType::Comma {
                break;
            }
            self.next_token(); // `,` を読み飛ばす
        }
        if self.cur_token.token_type != TokenType::Rbrace {
            return Err(Error { errors: vec![format!("[parse_array_initializer_expression] expected next token to be Rbrace, got {:?}", self.cur_token.token_type)] });
        }
        return Ok(ast::Expression::ArrayInitializerExpression { elements });
    }

    // !, -, ++, --, *, & の前置演算子をパースする
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

    fn infix(&mut self, left: Expression) -> Result<ast::Expression> {
        return if self.cur_token.token_type == TokenType::Lparem {
            self.parse_function_call_expression(left)
        } else if self.cur_token.token_type == TokenType::Lbracket {
            self.parse_index_expression(left)
        } else {
            self.parse_infix_expression(left)
        };
    }

    fn parse_function_call_expression(&mut self, left: Expression) -> Result<ast::Expression> {
        let function_name = match left {
            ast::Expression::Identifier { value } => value,
            _ => {
                return Err(Error { errors: vec![format!("[parse_function_call_expression] expected `left` to be Expression::Identifier, got {:?}", left)] });
            }
        };
        let mut arguments: Vec<ast::Expression> = vec![];
        self.next_token(); // `(` を読み飛ばす
        if self.cur_token.token_type != TokenType::Rparem {
            loop {
                let arg = self.parse_expression(ExpressionPrecedence::Lowest)?;
                arguments.push(arg);
                if self.peek_token.token_type != TokenType::Comma {
                    self.next_token(); // cur_token is `)`
                    break;
                }
                self.next_token(); // cur_token is `,`
                self.next_token(); // `,` を読み飛ばす
            }
        }
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_function_call_expression] expected next token to be Rparem, got {:?}", self.cur_token.token_type)] });
        }
        return Ok(ast::Expression::FunctionCallExpression { function_name, arguments });
    }

    fn parse_index_expression(&mut self, left: Expression) -> Result<ast::Expression> {
        self.next_token();
        let right = self.parse_expression(ExpressionPrecedence::Lowest)?;
        self.next_token();
        if self.cur_token.token_type != TokenType::Rbracket {
            return Err(Error { errors: vec![format!("[parse_index_expression] expected next token to be Rbracket, got {:?}", self.cur_token.token_type)] });
        }
        return Ok(ast::Expression::IndexExpression {
            left: Box::new(left),
            index: Box::new(right),
        });
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

    // ++, -- の後置演算子をパースする
    fn parse_postfix_expression(&mut self, left: Expression) -> ast::Expression {
        let operator = self.cur_token.literal.clone();
        return ast::Expression::PostfixExpression {
            operator,
            left: Box::new(left),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::ast::{Expression, TypeRef};

    use super::*;

    #[test]
    fn test_external_vardecl() {
        // given
        let input = "
int x = 10;
int x;
int x, y, z;
";
        let expected = vec![
            vec![("int", "x", Some("10".to_string()))],
            vec![("int", "x", None)],
            vec![("int", "x", None), ("int", "y", None), ("int", "z", None)],
        ];

        // when
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let mut external_items: Vec<ast::ExternalItem> = vec![];
        let rows_count = 3;
        for _ in 0..rows_count {
            external_items.push(p.parse_external_item().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(external_items.len(), rows_count);
        for (i, item) in external_items.iter().enumerate() {
            match item {
                ast::ExternalItem::VarDecl { declarators } => {
                    for (j, (type_dec, declarator)) in declarators.iter().enumerate() {
                        let (expected_type, expected_name, expected_value) = &expected[i][j];
                        assert_eq!(type_dec.type_name(), expected_type.to_string());
                        assert_eq!(declarator.name, *expected_name);
                        assert_eq!(declarator.value.as_ref().map(|x| x.to_string()), *expected_value);
                    }
                }
                _ => panic!("ExternalItem is not VarDecl"),
            }
        }
    }

    #[test]
    fn test_external_functiondecl() {
        // given
        let input = "
int add(int a, int b) {
    return a + b;
}
hoge() {
}

Person createPerson(int age);

int foo(int *as);
int bar(int as[]);
";
        let expected = vec![
            (
                "int",
                "add",
                vec![
                    ast::Parameter {
                        type_dec: ast::TypeRef::Named("int".to_string()),
                        name: "a".to_string(),
                    },
                    ast::Parameter {
                        type_dec: ast::TypeRef::Named("int".to_string()),
                        name: "b".to_string(),
                    }
                ],
                Some("{\n    return (a + b);\n}".to_string()),
            ),
            (
                "int",
                "hoge",
                vec![],
                Some("{\n}".to_string()),
            ),
            (
                "Person",
                "createPerson",
                vec![
                    ast::Parameter {
                        type_dec: ast::TypeRef::Named("int".to_string()),
                        name: "age".to_string(),
                    },
                ],
                None,
            ),
            (
                "int",
                "foo",
                vec![
                    ast::Parameter {
                        type_dec: ast::TypeRef::Pointer(
                            Box::new(ast::TypeRef::Named("int".to_string())),
                        ),
                        name: "as".to_string(),
                    },
                ],
                None,
            ),
            (
                "int",
                "bar",
                vec![
                    ast::Parameter {
                        type_dec: ast::TypeRef::Array(
                            Box::new(ast::TypeRef::Named("int".to_string())),
                            None,
                        ),
                        name: "as".to_string(),
                    },
                ],
                None,
            ),
        ];

        // when
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let mut external_items: Vec<ast::ExternalItem> = vec![];
        let rows_count = 5;
        for _ in 0..rows_count {
            external_items.push(p.parse_external_item().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(external_items.len(), rows_count);
        for (i, item) in external_items.iter().enumerate() {
            match item {
                ast::ExternalItem::FunctionDecl { return_type_dec, name, parameters, body } => {
                    let (expected_return_type, expected_name, expected_parameters, expected_body) = &expected[i];
                    assert_eq!(return_type_dec.type_name(), expected_return_type.to_string());
                    assert_eq!(*name, *expected_name);
                    assert_eq!(parameters, expected_parameters);
                    assert_eq!(body.as_ref().map(|x| x.to_string()), *expected_body);
                }
                _ => panic!("ExternalItem is not FunctionDecl"),
            }
        }
    }

    #[test]
    fn test_vardecl() {
        // given
        let input = "
int five = 5;
int x = 10;
int x;
int x, y, z;
";
        let expected = vec![
            vec![("int", "five", Some("5".to_string()))],
            vec![("int", "x", Some("10".to_string()))],
            vec![("int", "x", None)],
            vec![("int", "x", None), ("int", "y", None), ("int", "z", None)],
        ];

        // when
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 4;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
            match stmt {
                ast::Statement::VarDecl { declarators } => {
                    for (j, (type_dec, declarator)) in declarators.iter().enumerate() {
                        let (expected_type, expected_name, expected_value) = &expected[i][j];
                        assert_eq!(type_dec.type_name(), expected_type.to_string());
                        assert_eq!(declarator.name, *expected_name);
                        assert_eq!(declarator.value.as_ref().map(|x| x.to_string()), *expected_value);
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
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 2;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
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
x;
foobar;
";
        let expected = vec!["x", "foobar"];

        // when
        let mut p = Parser::new(lexer::Lexer::new(input));
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 2;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
            let expected_value= expected[i];
            match stmt {
                ast::Statement::ExpressionStatement { expression } => {
                    assert_eq!(expression, &Expression::Identifier { value: expected_value.to_string() });
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
            ("!", ast::Expression::Identifier { value: "foobar".to_string() }),
            ("-", ast::Expression::Int { value: 5 }),
            ("++", ast::Expression::Identifier { value: "a".to_string() }),
        ];

        // when
        let mut p = Parser::new(lexer::Lexer::new(input));
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 3;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
            let (expected_operator, expected_right)= &expected[i];
            match stmt {
                ast::Statement::ExpressionStatement { expression: ast::Expression::PrefixExpression { operator, right } } => {
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
            ("+", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("-", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("*", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("/", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("%", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("==", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("!=", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            (">", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("<", ast::Expression::Int { value: 5 }, ast::Expression::Int { value: 6 }),
            ("=", ast::Expression::Identifier { value: "xyz".to_string() }, ast::Expression::Int { value: 10 }),
            ("+=", ast::Expression::Identifier { value: "a".to_string() }, ast::Expression::Int { value: 3 }),
            ("-=", ast::Expression::Identifier { value: "a".to_string() }, ast::Expression::Int { value: 3 }),
            ("*=", ast::Expression::Identifier { value: "a".to_string() }, ast::Expression::Int { value: 3 }),
            ("/=", ast::Expression::Identifier { value: "a".to_string() }, ast::Expression::Int { value: 3 }),
            ("%=", ast::Expression::Identifier { value: "a".to_string() }, ast::Expression::Int { value: 3 }),
        ];

        // when
        let mut p = Parser::new(lexer::Lexer::new(input));
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 15;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
            let (expected_operator, expected_left, expected_right)= &expected[i];
            match stmt {
                ast::Statement::ExpressionStatement { expression: ast::Expression::InfixExpression { operator, left, right } } => {
                    assert_eq!(operator, expected_operator);
                    assert_eq!(left.as_ref(), expected_left);
                    assert_eq!(right.as_ref(), expected_right);
                }
                _ => panic!("Statement is not ExpressionStatement"),
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
        let mut p = Parser::new(lexer::Lexer::new(input));
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 3;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
            let (expected_function_name, expected_arguments)= &expected[i];
            match stmt {
                ast::Statement::ExpressionStatement { expression: ast::Expression::FunctionCallExpression { function_name, arguments } } => {
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
            ("++", ast::Expression::Identifier { value: "i".to_string() }),
            ("--", ast::Expression::Identifier { value: "a".to_string() }),
        ];

        // when
        let mut p = Parser::new(lexer::Lexer::new(input));
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 2;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
            let (expected_operator, expected_right)= &expected[i];
            match stmt {
                ast::Statement::ExpressionStatement { expression: ast::Expression::PostfixExpression { operator, left } } => {
                    assert_eq!(operator, expected_operator);
                    assert_eq!(left.as_ref(), expected_right);
                }
                _ => panic!("Statement is not PostfixExpression"),
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
            ("*ip + 10;", "((*ip) + 10);"),
            ("10 * *pn;", "(10 * (*pn));"),
            ("*(&x) + 1;", "((*(&x)) + 1);"),
            ("++*ip;", "(++(*ip));"),
            ("*ip++;", "(*(ip++));"),
            ("(*ip)++;", "((*ip)++);"),
            ("a[1] + 3;", "((a[1]) + 3);"),
            ("&a[0];", "(&(a[0]));"),
            ("a[x + i];", "(a[(x + i)]);"),
        ];
        for (input, expected) in tests.iter() {
            // when
            let mut p = Parser::new(lexer::Lexer::new(input));
            let stmt = p.parse_statement().unwrap();
            p.next_token();

            // then
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
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 4;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
            let (expected_condition, expected_consequence, expected_alternative) = expected[i];
            match stmt {
                ast::Statement::If { condition, consequence, alternative } => {
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
        let mut p = Parser::new(lexer::Lexer::new(input));
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 2;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        let stmt1 = statements.first().unwrap();
        let stmt2 = statements.get(1).unwrap();

        // 1つ目のswitch文のチェック
        if let ast::Statement::Switch { condition, switch_block } = stmt1 {
            assert_eq!(condition.to_string(), "a");
            // 各ラベルのチェック
            assert_eq!(switch_block.label_entries.len(), 4);
            assert_eq!(switch_block.label_entries[0].labels[0], SwitchLabel::Case(Expression::Int { value: 1 }));
            assert_eq!(switch_block.label_entries[0].start_index, 0);
            assert_eq!(switch_block.label_entries[1].labels[0], SwitchLabel::Case(Expression::Int { value: 2 }));
            assert_eq!(switch_block.label_entries[1].start_index, 2);
            assert_eq!(switch_block.label_entries[2].labels[0], SwitchLabel::Case(Expression::Int { value: 3 }));
            assert_eq!(switch_block.label_entries[2].start_index, 4);
            assert_eq!(switch_block.label_entries[3].labels[0], SwitchLabel::Default);
            assert_eq!(switch_block.label_entries[3].start_index, 6);

            assert_eq!(switch_block.body.len(), 7);
            if let Statement::ExpressionStatement { expression } = &switch_block.body[0] {
                assert_eq!(expression, &Expression::Identifier { value: "x".to_string() });
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
            match &switch_block.body[1] {
                Statement::Break => {}
                _ => panic!("Expected Statement with Break"),
            }
            if let Statement::ExpressionStatement { expression } = &switch_block.body[2] {
                assert_eq!(expression, &Expression::Identifier { value: "y".to_string() });
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
            match &switch_block.body[3] {
                Statement::Break => {}
                _ => panic!("Expected Statement with Break"),
            }
            if let Statement::ExpressionStatement { expression } = &switch_block.body[4] {
                assert_eq!(expression, &Expression::Identifier { value: "aaa".to_string() });
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
            match &switch_block.body[5] {
                Statement::Break => {}
                _ => panic!("Expected Statement with Break"),
            }
            if let Statement::ExpressionStatement { expression } = &switch_block.body[6] {
                assert_eq!(expression, &Expression::Identifier { value: "bbb".to_string() });
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
        } else {
            panic!("Statement is not Switch");
        }

        // 2つ目のswitch文のチェック
        if let ast::Statement::Switch { condition, switch_block } = stmt2 {
            assert_eq!(condition.to_string(), "x");
            // 各ラベルのチェック
            assert_eq!(switch_block.label_entries.len(), 2);
            assert_eq!(switch_block.label_entries[0].labels[0], SwitchLabel::Case(Expression::Int { value: 1 }));
            assert_eq!(switch_block.label_entries[0].labels[1], SwitchLabel::Case(Expression::Int { value: 2 }));
            assert_eq!(switch_block.label_entries[0].start_index, 0);
            assert_eq!(switch_block.label_entries[1].labels[0], SwitchLabel::Case(Expression::Int { value: 3 }));
            assert_eq!(switch_block.label_entries[1].start_index, 1);

            assert_eq!(switch_block.body.len(), 3);
            if let Statement::ExpressionStatement { expression } = &switch_block.body[0] {
                assert_eq!(expression, &Expression::Identifier { value: "bbb".to_string() });
            } else {
                panic!("Expected ExpressionStatement with Identifier");
            }
            if let Statement::ExpressionStatement { expression } = &switch_block.body[1] {
                assert_eq!(expression, &Expression::Identifier { value: "ccc".to_string() });
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
        let mut p = Parser::new(lexer::Lexer::new(input));
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 3;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), 3);
        for (i, stmt) in statements.iter().enumerate() {
            let (expected_condition, expected_body) = expected[i];
            match stmt {
                ast::Statement::While { condition, body} => {
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
        let mut p = Parser::new(lexer::Lexer::new(input));
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 2;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
            let (expected_body, expected_condition) = expected[i];
            match stmt {
                ast::Statement::DoWhile { body, condition } => {
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
        let mut p = Parser::new(lexer::Lexer::new(input));
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 3;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
            let (expected_init, expected_condition, expected_post, expected_body) = expected[i];
            match stmt {
                ast::Statement::For { init, condition, post, body} => {
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
        ];

        // when
        let l = lexer::Lexer::new(input);
        let mut p = Parser::new(l);
        let mut statements: Vec<ast::Statement> = vec![];
        let rows_count = 9;
        for _ in 0..rows_count {
            statements.push(p.parse_statement().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(statements.len(), rows_count);
        for (i, stmt) in statements.iter().enumerate() {
            match stmt {
                ast::Statement::VarDecl { declarators } => {
                    let (type_dec, declarator) = declarators.first().unwrap();
                    let (expected_type, expected_name, expected_value) = &expected[i];
                    assert_eq!(type_dec.type_name(), expected_type.to_string());
                    assert_eq!(declarator.name, *expected_name);
                    assert_eq!(declarator.value.as_ref().map(|x| x.to_string()), expected_value.map(|x|x.to_string()));
                }
                _ => panic!("Statement is not VarDecl"),
            }
        }
    }

}
