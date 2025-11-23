pub mod ast;
mod parser_for_expressions;
mod parser_for_external_items;
mod parser_for_statements;

use self::ast::{Declarator, TypeRef};
use self::parser_for_expressions::ExpressionPrecedence;
use crate::lexer::token::{Token, TokenType};
use crate::lexer::Lexer;

#[derive(Debug)]
struct Error {
    errors: Vec<String>,
}

type Result<T> = std::result::Result<T, Error>;

impl Token {
    fn loc(&self) -> ast::Loc {
        ast::Loc {
            row: self.row,
            col: self.col,
        }
    }
}

pub struct Parser {
    l: Lexer,

    /// 現在調べているトークン
    cur_token: Token,

    /// curTokenの次のトークン
    peek_token: Token,

    errors: Vec<String>,
}

impl Parser {
    pub(crate) fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            l,
            cur_token: Token::new(),
            peek_token: Token::new(),
            errors: vec![],
        };

        p.next_token();
        p.next_token();
        p
    }

    pub fn parse_program(&mut self) -> ast::Program {
        let mut program = ast::Program {
            external_item_nodes: vec![],
        };
        while self.cur_token.token_type != TokenType::Eof {
            let external_item = self.parse_external_item();
            match external_item {
                Ok(item) => {
                    program.external_item_nodes.push(item);
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
        if !self.errors.is_empty() {
            panic!("parse error: \n{}", self.errors.join("\n"));
        }
        program
    }

    fn error(&self, msg: String) -> Error {
        Error {
            errors: vec![format!(
                "error:{}:{}: {}",
                self.cur_token.row, self.cur_token.col, msg
            )],
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.l.next_token();
        println!(
            "#### current token_type:{:?}, literal:{}, peek token_type:{:?} ####",
            self.cur_token.token_type,
            self.cur_token.literal(),
            self.peek_token.token_type
        );
    }

    fn parse_struct_type(&mut self) -> Result<ast::TypeRef> {
        if self.cur_token.token_type != TokenType::Struct {
            return Err(self.error(format!(
                "[parse_struct] expected current token to be Struct, got {:?}",
                self.peek_token.token_type
            )));
        }
        self.next_token();

        let mut tag_name: Option<String> = None;
        if self.cur_token.token_type != TokenType::Lbrace {
            tag_name = Some(self.cur_token.literal());
            self.next_token();
        }

        let mut members: Vec<ast::StructDecl> = vec![];
        if self.cur_token.token_type == TokenType::Lbrace {
            // int x; int y; ...
            members = self.parse_struct_decls()?;
            if self.cur_token.token_type != TokenType::Rbrace {
                return Err(self.error(format!(
                    "[parse_struct] expected current token to be Rbrace, got {:?}",
                    self.cur_token.token_type
                )));
            }
            self.next_token();
        }

        Ok(ast::TypeRef::Struct { tag_name, members })
    }

    fn parse_struct_decls(&mut self) -> Result<Vec<ast::StructDecl>> {
        let mut decls: Vec<ast::StructDecl> = vec![];
        if self.cur_token.token_type != TokenType::Lbrace {
            return Err(self.error(format!(
                "[parse_struct_decls] expected next token to be Lbrace, got {:?}",
                self.peek_token.token_type
            )));
        }
        self.next_token();
        if self.cur_token.token_type == TokenType::Rbrace {
            return Ok(decls);
        }
        decls = self.parse_type_decls(
            TokenType::Semicolon,
            TokenType::Rbrace,
            ast::StructDecl::new,
        )?;
        if self.cur_token.token_type != TokenType::Rbrace {
            return Err(self.error(format!(
                "[parse_struct_decls] expected next token to be Rbrace, got {:?}",
                self.cur_token.token_type
            )));
        }
        Ok(decls)
    }

    fn parse_type_decls<F, T>(
        &mut self,
        separator: TokenType,
        end_token: TokenType,
        f: F,
    ) -> Result<Vec<T>>
    where
        F: Fn((TypeRef, String)) -> T,
    {
        let mut result: Vec<T> = vec![];
        loop {
            // struct?
            let mut type_dec = if self.cur_token.token_type == TokenType::Struct {
                self.parse_struct_type()?
            } else {
                let t = ast::TypeRef::Named(self.cur_token.literal());
                self.next_token();
                t
            };

            // pointer?
            while self.cur_token.token_type == TokenType::Asterisk {
                type_dec = ast::TypeRef::Pointer(Box::new(type_dec));
                self.next_token();
            }

            if self.cur_token.token_type != TokenType::Ident {
                return Err(self.error(format!(
                    "[parse_type_decls] expected next token to be IDENT, got {:?}",
                    self.cur_token.token_type
                )));
            }
            let name = self.cur_token.literal();

            // array?
            while self.peek_token.token_type == TokenType::Lbracket {
                self.next_token(); // cur_token is `[`
                self.next_token();
                if self.cur_token.token_type != TokenType::Rbracket {
                    return Err(self.error(format!(
                        "[parse_type_decls] expected next token to be Rbracket, got {:?}",
                        self.cur_token.token_type
                    )));
                }
                type_dec = TypeRef::Array {
                    type_dec: Box::new(type_dec),
                    size: None,
                };
            }

            result.push(f((type_dec, name)));

            self.next_token();
            if self.cur_token.token_type != separator {
                break;
            }
            if self.peek_token.token_type == end_token {
                self.next_token();
                break;
            }
            self.next_token(); // read separator
        }
        Ok(result)
    }

    fn parse_declarators(&mut self, base_type_dec: &TypeRef) -> Result<Vec<(TypeRef, Declarator)>> {
        let base_type_name = match base_type_dec {
            TypeRef::Named(name) => name.to_string(),
            TypeRef::Struct {
                tag_name: _,
                members: _,
            } => base_type_dec.type_name(),
            _ => {
                return Err(self.error(format!("[parse_declarators] expected `base_type_dec` should be TypeRef::Named, got {:?}", base_type_dec)));
            }
        };
        let mut result: Vec<(TypeRef, Declarator)> = vec![];
        loop {
            let mut type_dec = TypeRef::Named(base_type_name.clone());

            // pointer?
            while self.cur_token.token_type == TokenType::Asterisk {
                type_dec = ast::TypeRef::Pointer(Box::new(type_dec));
                self.next_token();
            }

            if self.cur_token.token_type != TokenType::Ident {
                return Err(self.error(format!(
                    "[parse_declarators] expected next token to be IDENT, got {:?}",
                    self.cur_token.token_type
                )));
            }
            let name = self.cur_token.literal();

            // array?
            while self.peek_token.token_type == TokenType::Lbracket {
                self.next_token(); // cur_token is `[`
                self.next_token();
                if self.cur_token.token_type == TokenType::Rbracket {
                    // <type_ref> <ident>[] = {<expression>, <expression>, ...};
                    type_dec = TypeRef::Array {
                        type_dec: Box::new(type_dec),
                        size: None,
                    };
                } else {
                    // <type_ref> <ident>[<size>];
                    if self.cur_token.token_type != TokenType::Integer {
                        return Err(self.error(format!(
                            "[parse_declarators] expected next token to be Integer, got {:?}",
                            self.cur_token.token_type
                        )));
                    }
                    let size = self.cur_token.literal().parse::<u32>().map_err(|_| Error {
                        errors: vec![format!(
                            "[parse_declarators] failed to parse integer size from {:?}",
                            self.cur_token.literal()
                        )],
                    })?;
                    type_dec = TypeRef::Array {
                        type_dec: Box::new(type_dec),
                        size: Some(size),
                    };
                    self.next_token();
                    if self.cur_token.token_type != TokenType::Rbracket {
                        return Err(self.error(format!(
                            "[parse_declarators] expected next token to be RBracket, got {:?}",
                            self.cur_token.token_type
                        )));
                    }
                }
            }

            let declarator = if self.peek_token.token_type != TokenType::Assign {
                // 値の指定がない、かつ、サイズが指定されていない配列型の場合はエラー
                if let TypeRef::Array {
                    type_dec: _,
                    size: None,
                } = type_dec
                {
                    return Err(self.error(format!(
                        "[parse_declarators] expected next token to be Assign, got {:?}",
                        self.peek_token.token_type
                    )));
                }
                ast::Declarator { name, value: None }
            } else {
                self.next_token(); // cur_token is `=`
                self.next_token(); // `=` を読み飛ばす
                let value = self.parse_expression(ExpressionPrecedence::Lowest)?;
                ast::Declarator {
                    name,
                    value: Some(value),
                }
            };

            result.push((type_dec, declarator));
            self.next_token();
            if self.cur_token.token_type != TokenType::Comma {
                break;
            }
            self.next_token(); // `,` を読み飛ばす
        }
        Ok(result)
    }
}
