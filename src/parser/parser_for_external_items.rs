use crate::parser::ast::Function;

use super::{Parser, Result, Error, TokenType};
use super::ast::{ExternalItem, TypeRef, Parameter};

impl Parser {
    pub(super) fn parse_external_item(&mut self) -> Result<ExternalItem> {
        let mut struct_type_dec: Option<TypeRef> = None;
        if self.cur_token.token_type == TokenType::Struct {
            let struct_type = self.parse_struct_type()?;
            if self.cur_token.token_type == TokenType::Semicolon {
                return Ok(ExternalItem::Struct(struct_type));
            }
            struct_type_dec = if self.cur_token.token_type == TokenType::Asterisk {
                self.next_token();
                Some(TypeRef::Pointer(Box::new(struct_type)))
            } else {
                Some(struct_type)
            };
        }

        match self.cur_token.token_type {
            TokenType::Int | TokenType::Void | TokenType::Char | TokenType::Short | TokenType::Long | TokenType::Ident => {
                let type_dec = 
                    struct_type_dec.unwrap_or_else(|| {
                        // 関数宣言で、返り値の型を省略している場合を考慮する
                        if self.peek_token.token_type == TokenType::Lparem {
                            // 省略されている場合は、int型とする
                            TypeRef::Named("int".to_string())
                        } else {
                            let tmp = TypeRef::Named(self.cur_token.literal());
                            self.next_token();
                            tmp
                        }
                    });

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

    fn parse_external_vardecl(&mut self, type_dec: TypeRef) -> Result<ExternalItem> {
        let declarators = self.parse_declarators(&type_dec)?;
        if self.cur_token.token_type != TokenType::Semicolon {
            return Err(Error { errors: vec![format!("[parse_external_item] expected next token to be Semicolon, got {:?}", self.peek_token.token_type)] });
        }
        return Ok(ExternalItem::VarDecl(declarators));
    }

    fn parse_external_function(&mut self, return_type_dec: TypeRef) -> Result<ExternalItem> {
        if self.cur_token.token_type != TokenType::Ident {
            return Err(Error { errors: vec![format!("[parse_external_function] expected next token to be IDENT, got {:?}", self.peek_token.token_type)] });
        }
        let name = self.cur_token.literal();

        self.next_token(); // cur_token is Lparam
        let parameters = self.parse_function_params()?;
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_external_function] expected next token to be Rparem, got {:?}", self.peek_token.token_type)] });
        }
        self.next_token();
        return if self.cur_token.token_type == TokenType::Lbrace {
            let body = self.parse_block_statement()?;
            Ok(ExternalItem::FunctionDecl(Function { return_type_dec, name, parameters, body: Some(Box::new(body)) }))
        } else {
            Ok(ExternalItem::FunctionDecl(Function { return_type_dec, name, parameters, body: None }))
        };
    }

    fn parse_function_params(&mut self) -> Result<Vec<Parameter>> {
        let mut parameters: Vec<Parameter> = vec![];
        if self.cur_token.token_type != TokenType::Lparem {
            return Err(Error { errors: vec![format!("[parse_function_params] expected next token to be Lparem, got {:?}", self.peek_token.token_type)] });
        }
        self.next_token();
        if self.cur_token.token_type == TokenType::Rparem {
            return Ok(parameters);
        }
        parameters = self.parse_type_decls(
            TokenType::Comma,
            TokenType::Rparem,
            Parameter::new,
        )?;
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(Error { errors: vec![format!("[parse_function_params] expected next token to be Rparem, got {:?}", self.cur_token.token_type)] });
        }
        return Ok(parameters);
    }

}

#[cfg(test)]
mod tests {

    use crate::lexer::Lexer;
    use super::*;

    #[test]
    fn test_external_vardecl() {
        // given
        let input = "
int x = 10;
int x;
int x, y, z;
char c1;
char c2 = '\n';
";
        let expected = vec![
            vec![("int", "x", Some("10".to_string()))],
            vec![("int", "x", None)],
            vec![("int", "x", None), ("int", "y", None), ("int", "z", None)],
            vec![("char", "c1", None)],
            vec![("char", "c2", Some("\n".to_string()))],
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<ExternalItem> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_external_item().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, item) in parse_results.iter().enumerate() {
            match item {
                ExternalItem::VarDecl(declarators)  => {
                    for (i, (type_dec, declarator)) in declarators.iter().enumerate() {
                        let (expected_type, expected_name, expected_value) = &expected[row_num][i];
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

int foo(int *as);
int bar(int as[]);

struct point addpoint(struct point p, struct point q);
struct point* movepoint(struct point* p, int x, int y);
";
        let expected = vec![
            (
                "int",
                "add",
                vec![
                    Parameter {
                        type_dec: TypeRef::Named("int".to_string()),
                        name: "a".to_string(),
                    },
                    Parameter {
                        type_dec: TypeRef::Named("int".to_string()),
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
                "int",
                "foo",
                vec![
                    Parameter {
                        type_dec: TypeRef::Pointer(
                            Box::new(TypeRef::Named("int".to_string())),
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
                    Parameter {
                        type_dec: TypeRef::Array{
                            type_dec: Box::new(TypeRef::Named("int".to_string())),
                            size: None,
                        },
                        name: "as".to_string(),
                    },
                ],
                None,
            ),
            (
                "struct point",
                "addpoint",
                vec![
                    Parameter {
                        type_dec: TypeRef::Struct { tag_name: Some("point".to_string()), members: vec![], },
                        name: "p".to_string(),
                    },
                    Parameter {
                        type_dec: TypeRef::Struct { tag_name: Some("point".to_string()), members: vec![], },
                        name: "q".to_string(),
                    },
                ],
                None,
            ),
            (
                "struct point*",
                "movepoint",
                vec![
                    Parameter {
                        type_dec: TypeRef::Pointer(Box::new(TypeRef::Struct { tag_name: Some("point".to_string()), members: vec![], })),
                        name: "p".to_string(),
                    },
                    Parameter {
                        type_dec: TypeRef::Named("int".to_string()),
                        name: "x".to_string(),
                    },
                    Parameter {
                        type_dec: TypeRef::Named("int".to_string()),
                        name: "y".to_string(),
                    }
                ],
                None,
            ),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<ExternalItem> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_external_item().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, item) in parse_results.iter().enumerate() {
            match item {
                ExternalItem::FunctionDecl(Function { return_type_dec, name, parameters, body }) => {
                    let (expected_return_type, expected_name, expected_parameters, expected_body) = &expected[row_num];
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
    fn test_external_structdecl() {
        // given
        let input = "
struct point {
    int x;
    int y;
};
struct key {
    char *word;
    int count;
};
";
        let expected = vec![
            ("struct", "int x; int y", Some("point")),
            ("struct", "char* word; int count", Some("key")),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<ExternalItem> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_external_item().unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, item) in parse_results.iter().enumerate() {
            match item {
                ExternalItem::Struct(TypeRef::Struct { tag_name, members }) => {
                    let (_, expected_members, expected_tag_name) = &expected[row_num];
                    assert_eq!(tag_name.as_ref().map(|x| x.to_string()).unwrap_or("".to_string()), expected_tag_name.unwrap_or(""));
                    assert_eq!(members.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("; "), expected_members.to_string());
                }
                _ => panic!("Statement is not Struct"),
            }
        }
    }
}
