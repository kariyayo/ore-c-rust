use crate::parser::ast::StructRef;

use super::ast::{ExternalItem, ExternalItemNode, FunctionDecl, Parameter, StructDecl, TypeRef};
use super::{Parser, Result, TokenType};

impl Parser {
    pub(super) fn parse_external_item(&mut self) -> Result<ExternalItemNode> {
        let mut struct_type_ref: Option<TypeRef> = None;

        if self.cur_token.token_type == TokenType::Struct
            || self.cur_token.token_type == TokenType::TypeDef
        {
            let is_typedef = self.cur_token.token_type == TokenType::TypeDef;
            if is_typedef {
                self.next_token();
            }
            let (tag_name, members) = self.parse_struct_type()?;

            if !is_typedef && self.cur_token.token_type == TokenType::Semicolon {
                return Ok((
                    ExternalItem::StructDeclNode(StructDecl { tag_name, members }),
                    self.cur_token.loc(),
                ));
            }

            if is_typedef {
                let ty =
                    TypeRef::TypeAlias(Box::new(TypeRef::Struct(StructRef::Decl(StructDecl {
                        tag_name,
                        members,
                    }))));
                let mut aliases: Vec<String> = vec![];
                loop {
                    if self.cur_token.token_type != TokenType::Ident {
                        return Err(self.error(format!(
                            "[parse_external_item] expected next token to be IDENT, got {:?}",
                            self.cur_token.token_type
                        )));
                    }
                    let name = self.cur_token.literal();
                    aliases.push(name);

                    self.next_token();
                    if self.cur_token.token_type != TokenType::Comma {
                        break;
                    }
                }
                return Ok((ExternalItem::TypeRefNode(ty, aliases), self.cur_token.loc()));
            }

            let type_ref = if !members.is_empty() {
                Some(TypeRef::Struct(StructRef::Decl(StructDecl {
                    tag_name: tag_name.clone(),
                    members,
                })))
            } else if tag_name.is_some() {
                Some(TypeRef::Struct(StructRef::TagName(tag_name.unwrap())))
            } else {
                None
            };
            struct_type_ref = match type_ref {
                None => None,
                Some(ty) => {
                    if self.cur_token.token_type == TokenType::Asterisk {
                        self.next_token();
                        Some(TypeRef::Pointer(Box::new(ty)))
                    } else {
                        Some(ty)
                    }
                }
            };
        }

        match self.cur_token.token_type {
            TokenType::Int
            | TokenType::Void
            | TokenType::Char
            | TokenType::Short
            | TokenType::Long
            | TokenType::Ident => {
                let type_ref = struct_type_ref.unwrap_or_else(|| {
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
                    self.parse_external_function(type_ref)
                } else {
                    // 変数
                    self.parse_external_vardecl(type_ref)
                }
            }
            _ => Err(self.error(format!(
                "[parse_external_item] expected external item token, got {:?}",
                self.cur_token.token_type
            ))),
        }
    }

    fn parse_external_vardecl(&mut self, type_dec: TypeRef) -> Result<ExternalItemNode> {
        let loc = self.cur_token.loc();
        let declarators = self.parse_declarators(&type_dec)?;
        if self.cur_token.token_type != TokenType::Semicolon {
            return Err(self.error(format!(
                "[parse_external_item] expected next token to be Semicolon, got {:?}",
                self.peek_token.token_type
            )));
        }
        Ok((ExternalItem::VarDeclNode(declarators), loc))
    }

    fn parse_external_function(&mut self, return_type_ref: TypeRef) -> Result<ExternalItemNode> {
        let loc = self.cur_token.loc();
        if self.cur_token.token_type != TokenType::Ident {
            return Err(self.error(format!(
                "[parse_external_function] expected next token to be IDENT, got {:?}",
                self.peek_token.token_type
            )));
        }
        let name = self.cur_token.literal();

        self.next_token(); // cur_token is Lparam
        let parameters = self.parse_function_params()?;
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(self.error(format!(
                "[parse_external_function] expected next token to be Rparem, got {:?}",
                self.peek_token.token_type
            )));
        }
        self.next_token();
        if self.cur_token.token_type == TokenType::Lbrace {
            let body = self.parse_block_statement()?;
            Ok((
                ExternalItem::FunctionDeclNode(FunctionDecl {
                    return_type_ref,
                    name,
                    parameters,
                    body: Some(Box::new(body)),
                }),
                loc,
            ))
        } else {
            Ok((
                ExternalItem::FunctionDeclNode(FunctionDecl {
                    return_type_ref,
                    name,
                    parameters,
                    body: None,
                }),
                loc,
            ))
        }
    }

    fn parse_function_params(&mut self) -> Result<Vec<Parameter>> {
        let mut parameters: Vec<Parameter> = vec![];
        if self.cur_token.token_type != TokenType::Lparem {
            return Err(self.error(format!(
                "[parse_function_params] expected next token to be Lparem, got {:?}",
                self.peek_token.token_type
            )));
        }
        self.next_token();
        if self.cur_token.token_type == TokenType::Rparem {
            return Ok(parameters);
        }
        parameters = self.parse_type_refs(TokenType::Comma, TokenType::Rparem, Parameter::new)?;
        if self.cur_token.token_type != TokenType::Rparem {
            return Err(self.error(format!(
                "[parse_function_params] expected next token to be Rparem, got {:?}",
                self.cur_token.token_type
            )));
        }
        Ok(parameters)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

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
            parse_results.push(p.parse_external_item().map(|(item, _)| item).unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, item) in parse_results.iter().enumerate() {
            match item {
                ExternalItem::VarDeclNode(declarators) => {
                    for (i, (type_dec, declarator)) in declarators.iter().enumerate() {
                        let (expected_type, expected_name, expected_value) = &expected[row_num][i];
                        assert_eq!(type_dec.type_name(), expected_type.to_string());
                        assert_eq!(declarator.name, *expected_name);
                        assert_eq!(
                            declarator.value.as_ref().map(|x| x.0.to_string()),
                            *expected_value
                        );
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
                        type_ref: TypeRef::Named("int".to_string()),
                        name: "a".to_string(),
                    },
                    Parameter {
                        type_ref: TypeRef::Named("int".to_string()),
                        name: "b".to_string(),
                    },
                ],
                Some("{\n    return (a + b);\n}".to_string()),
            ),
            ("int", "hoge", vec![], Some("{\n}".to_string())),
            (
                "int",
                "foo",
                vec![Parameter {
                    type_ref: TypeRef::Pointer(Box::new(TypeRef::Named("int".to_string()))),
                    name: "as".to_string(),
                }],
                None,
            ),
            (
                "int",
                "bar",
                vec![Parameter {
                    type_ref: TypeRef::Array {
                        type_ref: Box::new(TypeRef::Named("int".to_string())),
                        size: None,
                    },
                    name: "as".to_string(),
                }],
                None,
            ),
            (
                "struct point",
                "addpoint",
                vec![
                    Parameter {
                        type_ref: TypeRef::Struct(StructRef::TagName("point".to_string())),
                        name: "p".to_string(),
                    },
                    Parameter {
                        type_ref: TypeRef::Struct(StructRef::TagName("point".to_string())),
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
                        type_ref: TypeRef::Pointer(Box::new(TypeRef::Struct(StructRef::TagName(
                            "point".to_string(),
                        )))),
                        name: "p".to_string(),
                    },
                    Parameter {
                        type_ref: TypeRef::Named("int".to_string()),
                        name: "x".to_string(),
                    },
                    Parameter {
                        type_ref: TypeRef::Named("int".to_string()),
                        name: "y".to_string(),
                    },
                ],
                None,
            ),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<ExternalItem> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_external_item().map(|(item, _)| item).unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, item) in parse_results.iter().enumerate() {
            match item {
                ExternalItem::FunctionDeclNode(FunctionDecl {
                    return_type_ref: return_type_dec,
                    name,
                    parameters,
                    body,
                }) => {
                    let (expected_return_type, expected_name, expected_parameters, expected_body) =
                        &expected[row_num];
                    assert_eq!(
                        return_type_dec.type_name(),
                        expected_return_type.to_string(),
                        "row_num: {}",
                        row_num,
                    );
                    assert_eq!(*name, *expected_name, "row_num: {}", row_num);
                    assert_eq!(parameters, expected_parameters, "row_num: {}", row_num);
                    assert_eq!(
                        body.as_ref().map(|x| x.0.to_string()),
                        *expected_body,
                        "row_num: {}",
                        row_num
                    );
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
            parse_results.push(p.parse_external_item().map(|(item, _)| item).unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, item) in parse_results.iter().enumerate() {
            match item {
                ExternalItem::StructDeclNode(StructDecl { tag_name, members }) => {
                    let (_, expected_members, expected_tag_name) = &expected[row_num];
                    assert_eq!(
                        tag_name
                            .as_ref()
                            .map(|x| x.to_string())
                            .unwrap_or("".to_string()),
                        expected_tag_name.unwrap_or("")
                    );
                    assert_eq!(
                        members
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<String>>()
                            .join("; "),
                        expected_members.to_string()
                    );
                }
                _ => panic!("Statement is not Struct"),
            }
        }
    }

    #[test]
    fn test_invalid_external_return() {
        // given
        let input = "
int x = 10;
return x;
";
        let mut p = Parser::new(Lexer::new(input));

        // when
        let res = p.parse_external_item().map(|(item, _)| item).unwrap();
        p.next_token();

        // then
        if let ExternalItem::VarDeclNode(declarators) = res {
            let (type_dec, declarator) = &declarators[0];
            let (expected_type, expected_name, expected_value) = ("int", "x", "10".to_string());
            assert_eq!(type_dec.type_name(), expected_type.to_string());
            assert_eq!(declarator.name, *expected_name);
            assert_eq!(
                declarator.value.as_ref().map(|x| x.0.to_string()),
                Some(expected_value)
            );
        }

        // when
        let e = p.parse_external_item().err().unwrap();
        p.next_token();

        assert_eq!(
            e.errors[0],
            "error:3:1: [parse_external_item] expected external item token, got Return"
        );
    }

    #[test]
    fn test_external_typedef() {
        // given
        let input = "
typedef struct point {
    int x;
    int y;
} point;
typedef struct point_tag {
    int x;
    int y;
} point;
typedef struct {
    int x;
    int y;
} point;
";
        let expected = vec![
            ("struct", "int x; int y", Some("point"), Some("point")),
            ("struct", "int x; int y", Some("point_tag"), Some("point")),
            ("struct", "int x; int y", None, Some("point")),
        ];

        // when
        let mut p = Parser::new(Lexer::new(input));
        let mut parse_results: Vec<ExternalItem> = vec![];
        for _ in 0..expected.len() {
            parse_results.push(p.parse_external_item().map(|(item, _)| item).unwrap());
            p.next_token();
        }

        // then
        assert_eq!(parse_results.len(), expected.len());
        for (row_num, item) in parse_results.iter().enumerate() {
            let (_, expected_members, expected_tag_name, expected_alias) = expected[row_num];
            match item {
                ExternalItem::TypeRefNode(TypeRef::TypeAlias(type_ref), names) => {
                    let TypeRef::Struct(StructRef::Decl(StructDecl { tag_name, members })) =
                        *type_ref.clone()
                    else {
                        panic!("type_ref is not Struct. type_ref: {:?}", type_ref);
                    };
                    assert_eq!(
                        tag_name.as_ref().unwrap_or(&"".to_string()),
                        expected_tag_name.unwrap_or("")
                    );
                    assert_eq!(
                        members
                            .iter()
                            .map(|x| x.to_string())
                            .collect::<Vec<String>>()
                            .join("; "),
                        expected_members.to_string()
                    );
                    assert_eq!(names[0], expected_alias.unwrap().to_string());
                }
                _ => panic!("item is not VarDeclNode. item: {:?}", item),
            }
        }
    }
}
