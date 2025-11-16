use std::{collections::{HashMap, HashSet}, fmt};

use crate::parser::ast::{Declarator, Expression, ExpressionNode, ExternalItem, Function, Loc, Parameter, Program, Statement, StatementNode, TypeRef};

#[derive(Debug)]
pub struct Error {
    errors: Vec<String>,
}

impl Error {
    fn new(loc: &Loc, msg: String) -> Error {
        return Error { errors: vec![build_error_msg(loc, msg)] };
    }
}

fn build_error_msg(loc: &Loc, msg: String) -> String {
    format!("error:{}:{}: {}", loc.row, loc.col, msg)
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for error in &self.errors {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

impl core::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct TypeTable {
    entities: HashSet<TypeRef>,
}

impl TypeTable {
    fn put(&mut self, type_ref: TypeRef) {
        self.entities.insert(type_ref);
    }
    fn find(&self, type_ref: &TypeRef) -> Option<TypeRef> {
        match type_ref {
            TypeRef::Named(_) | TypeRef::Struct { .. } => {
                self.entities.iter().find(|elm| { elm.type_name() == type_ref.type_name() }).cloned()
            },
            TypeRef::Pointer(inner_type_ref) => {
                self.find(inner_type_ref).map(|ty| TypeRef::Pointer(Box::new(ty)))
            }
            TypeRef::Array { type_dec, size } => {
                self.find(type_dec).map(|ty| TypeRef::Array { type_dec: Box::new(ty), size: *size })
            },
        }
    }
    fn contains(&self, type_ref: &TypeRef) -> bool {
        self.find(type_ref).is_some()
    }
}

#[derive(Debug)]
pub struct Functions {
    entities: HashMap<String, Function>,
}

impl Functions {
    fn put(&mut self, function: Function) {
        self.entities.insert(function.name.to_string(), function);
    }
    fn find(&self, name: &str) -> Option<Function> {
        self.entities.get(name).cloned()
    }
}

#[derive(Debug)]
struct LocalScope<'a> {
    parent: Option<&'a LocalScope<'a>>,
    entities: HashMap<String, TypeRef>,
}

impl <'a> LocalScope<'a> {
    fn put(&mut self, name: &str, type_ref: TypeRef) {
        self.entities.insert(name.to_string(), type_ref);
    }

    fn find(&self, name: &str) -> Option<TypeRef> {
        self.entities.get(name).cloned().or_else(|| {
            self.parent.and_then(|p| p.find(name))
        })
    }
}

#[derive(Debug)]
struct Env<'a> {
    type_table: &'a mut TypeTable,
    functions: &'a mut Functions,
    scope: LocalScope<'a>,
}

impl <'a> Env<'a> {
    fn put_type(&mut self, type_ref: TypeRef) {
        self.type_table.put(type_ref);
    }

    fn find_type(&self, type_ref: &TypeRef) -> Option<TypeRef> {
        self.type_table.find(type_ref)
    }

    fn is_defined_type(&self, type_ref: &TypeRef) -> bool {
        self.type_table.contains(type_ref)
    }

    fn put_function(&mut self, function: Function) {
        self.functions.put(function);
    }

    fn find_function(&self, name: &str) -> Option<Function> {
        self.functions.find(name)
    }

    fn put_vardecl(&mut self, name: &str, type_ref: TypeRef) {
        self.scope.put(name, type_ref);
    }

    fn find_vardecl(&self, name: &str) -> Option<TypeRef> {
        self.scope.find(name)
    }
}

pub fn check_type(ast: &Program) -> Result<()> {
    let mut type_table = TypeTable { entities: generate_c_types() };
    let mut functions = Functions { entities: HashMap::new() };
    let global_scope = LocalScope { parent: None, entities: HashMap::new() };

    let mut results: Vec<Error> = vec![];
    let mut env = Env { type_table: &mut type_table, functions: &mut functions, scope: global_scope };

    for item_node in &ast.external_item_nodes {
        let (item, external_item_loc) = item_node;
        match item {
            ExternalItem::Struct(type_ref) => {
                // 宣言と初期化子のフィールドをチェックする
                if let TypeRef::Struct { tag_name: _, members } = type_ref {
                    let es: Vec<Error> = members.iter().filter_map(|member| {
                        if env.is_defined_type(&member.type_dec) {
                            None
                        } else {
                            Some(Error::new(external_item_loc, format!("struct member's type is not defined: {}", member.type_dec.type_name())))
                        }
                    }).collect();
                    if es.len() > 0 {
                        results.extend(es);
                    } else {
                        env.put_type(type_ref.clone());
                    }
                } else {
                    panic!("ExternalItem::Struct type_ref should be TypeRef::Struct, but is {:?}", type_ref);
                }
            },
            ExternalItem::VarDecl(items) => {
                for (type_ref, decl) in items {
                    if let Err(e) = check_declarator(&mut env, type_ref, decl, external_item_loc) {
                        results.push(e);
                    } else {
                        env.put_vardecl(decl.name.as_str(), type_ref.clone());
                    }
                }
            },
            ExternalItem::FunctionDecl(function) => {
                let Function { return_type_dec, name, parameters, body } = function;
                if let Err(e) = check_function_declaration(&mut env, return_type_dec, name, parameters, body, external_item_loc) {
                    results.push(e);
                } else {
                    env.put_function(function.clone());
                }
            },
        }
    }
    if results.is_empty() {
        return Ok(());
    }
    Err(Error { errors: results.into_iter().flat_map(|e| e.errors).collect() })
}

fn check_statement(env: &mut Env, stmt_node: &StatementNode) -> Vec<Error> {
    let mut results: Vec<Error> = vec![];
    let (stmt, loc) = stmt_node;
    match stmt {
        Statement::Return(expression) => {
            if let Some(exp) = expression {
                if let Err(e) = check_expression(env, exp) {
                    results.push(e);
                }
            }
        },
        Statement::Break => todo!(),
        Statement::Continue => todo!(),
        Statement::VarDecl(items) => {
            for (type_ref, decl) in items {
                if let Err(e) = check_declarator(env, type_ref, decl, loc) {
                    results.push(e);
                } else {
                    env.put_vardecl(&decl.name, type_ref.clone());
                }
            }
        }
        Statement::Block(statements) => {
            let local_scope = LocalScope { parent: Some(&env.scope), entities: HashMap::new() };
            let mut new_env = Env { type_table: env.type_table, functions: env.functions, scope: local_scope };
            statements.iter().for_each(|stmt| {
                let mut errors = check_statement(&mut new_env, stmt);
                results.append(&mut errors);
            })
        },
        Statement::If { condition, consequence, alternative } => {
            match check_expression(env, condition) {
                Ok(condition_type) => {
                    if condition_type.type_name() != "int" {
                        results.push(Error::new(&condition.1, format!("type error. condition type is {}", condition_type.type_name())));
                    }
                    results.extend(check_statement(env, &consequence));
                    if let Some(alt_stmt) = alternative {
                        results.extend(check_statement(env, alt_stmt));
                    }
                },
                Err(e) => results.push(e),
            }
        },
        Statement::Switch { condition, switch_block } => todo!(),
        Statement::While { condition, body } => todo!(),
        Statement::DoWhile { body, condition } => todo!(),
        Statement::For { init, condition, post, body } => todo!(),
        Statement::ExpressionStatement(expression) => {
            if let Err(e) = check_expression(env, expression) {
                results.push(e);
            }
        },
    }
    results
}

fn check_expression(env: &Env, exp_node: &ExpressionNode) -> Result<TypeRef> {
    let (exp, exp_loc) = exp_node;
    match exp {
        Expression::Int(_) => {
            Ok(TypeRef::Named("int".to_string()))
        }
        Expression::CharacterLiteral(_) => {
            Ok(TypeRef::Named("char".to_string()))
        },
        Expression::StringLiteral(_) => {
            Ok(TypeRef::Pointer(Box::new(TypeRef::Named("char".to_string()))))
        },
        Expression::Identifier(name) => {
            match env.find_vardecl(name) {
                Some(type_ref) => {
                    env.find_type(&type_ref).ok_or(Error::new(exp_loc, format!("variable type `{}` is not defined", type_ref.type_name())))
                },
                None => {
                    Err(Error::new(exp_loc, format!("variable `{}` is not defined", name)))
                }
            }
        },
        Expression::PrefixExpression { operator, right } => {
            match operator.as_str() {
                "!" | "-" | "++" | "--" => todo!(),
                "*" => {
                    let right_type = check_expression(env, right)?;
                    let TypeRef::Pointer(ty) = right_type else {
                        return Err(Error::new(exp_loc, format!("cannot dereference non-pointer type `{}`", right_type.type_name())));
                    };
                    Ok(*ty)
                },
                "&" => {
                    let right_type = check_expression(env, right)?;
                    Ok(TypeRef::Pointer(Box::new(right_type)))
                },
                _ => panic!("prefix is not supported. prefix is {}, right is {:?}", operator, right),
            }
        },
        Expression::InfixExpression { operator, left, right } => {
            match operator.as_str() {
                "+" | "-" | "*" | "/" | "%" | "<" | ">" | "<=" | ">=" | "==" | "!=" => {
                    check_basic_calc_operator(env, left, right)
                },
                "." => {
                    let left_type = check_expression(env, left)?;
                    check_struct(env, &left_type, &left.as_ref().1, right)
                },
                "->" => {
                    let left_type = check_expression(env, left)?;
                    let TypeRef::Pointer(ty) = left_type else {
                        return Err(Error::new(
                            &left.as_ref().1,
                            format!( "left operand is not pointer. left is `{}`.", left_type.type_name()),
                        ));
                    };
                    check_struct(env, ty.as_ref(), &left.as_ref().1, right)
                },
                _ => {
                    let left_type = check_expression(env, left)?;
                    let right_type = check_expression(env, right)?;
                    if left_type != right_type {
                        Err(Error::new(
                            &left.as_ref().1,
                            format!(
                                "type mismatched for operator `{}`. left type is {}, right type is {}",
                                operator, left_type.type_name(), right_type.type_name()
                            ),
                        ))
                    } else {
                        Ok(left_type)
                    }
                }
            }
        },
        Expression::PostfixExpression { operator, left } => todo!(),
        Expression::FunctionCallExpression { function_name, arguments } => {
            let Some(f) = env.find_function(&function_name) else {
                return Err(Error::new(exp_loc, format!("function `{}` is not defined", function_name)));
            };
            if f.parameters.len() != arguments.len() {
                return Err(Error::new(exp_loc, format!("wrong number of arguments, `{}`.", function_name)))
            }
            let errors: Vec<String> = f.parameters.iter()
                .map(|p| env.find_type(&p.type_dec).unwrap_or_else(|| panic!("parameter type is not defined: {:?}", p.type_dec)))
                .zip(arguments.iter())
                .enumerate()
                .flat_map(|(i, (param, arg))| {
                    match check_expression(env, arg) {
                        Ok(arg_type) => {
                            if arg_type != param {
                                let (_, loc) = arg;
                                vec![build_error_msg(
                                    loc,
                                    format!( "mismatched type for argument {} in function call, `{}`.", i + 1, function_name)
                                )]
                            } else {
                                vec![]
                            }
                        },
                        Err(e) => e.errors,
                    }
                }).collect();
            if errors.is_empty() {
                Ok(f.return_type_dec.clone())
            } else {
                Err(Error { errors })
            }
        },
        Expression::InitializerExpression { elements } => todo!(),
        Expression::IndexExpression { left, index } => {
            // as[x]の `x` の型がintであることをチェック
            let index_type = check_expression(env, index)?;
            if index_type.type_name() != "int" {
                return Err(Error::new(
                    &index.as_ref().1,
                    format!( "index should be int. but it is `{}`.", index_type.type_name()),
                ));
            }
            // as[x]の `as` の型を解決する
            let var_type = check_expression(env, left)?;
            match var_type {
                TypeRef::Pointer(inner_type) => Ok(inner_type.as_ref().clone()),
                TypeRef::Array { type_dec, size } => Ok(type_dec.as_ref().clone()),
                _ => {
                    Err(Error::new(
                        &left.as_ref().1,
                        format!( "index operand should be array. but it is `{}`.", var_type.type_name()),
                    ))
                }
            }
        },
    }
} 

fn check_function_declaration(
    env: &mut Env,
    return_type_dec: &TypeRef,
    name: &str,
    parameters: &Vec<Parameter>,
    body: &Option<Box<StatementNode>>,
    loc: &Loc,
) -> Result<()> {
    let mut results: Vec<Error> = vec![];

    if !env.is_defined_type(return_type_dec) {
        results.push(Error::new(loc, format!("return type is not defined: {}", return_type_dec.type_name())));
    }

    let local_scope = LocalScope { parent: Some(&env.scope), entities: HashMap::new() };
    let mut new_env = Env { type_table: env.type_table, functions: env.functions, scope: local_scope };
    for p in parameters {
        if !new_env.is_defined_type(&p.type_dec) {
            results.push(Error::new(loc, format!("parameter type is not defined: {}", p.type_dec.type_name())));
        }
        new_env.put_vardecl(&p.name, p.type_dec.clone());
    }

    if let Some(stmt) = body {
        results.extend(check_statement(&mut new_env, stmt));
    }
    if results.is_empty() {
        return Ok(());
    }
    Err(Error { errors: results.into_iter().flat_map(|e| e.errors).collect() })
}

fn check_declarator(env: &Env, type_ref: &TypeRef, decl: &Declarator, external_loc: &Loc) -> Result<TypeRef> {
    let Some(exp) = &decl.value else {
        return Ok(type_ref.clone());
    };
    let Expression::InitializerExpression { elements: init_elms } = &exp.0 else {
        // 右辺が初期化子ではない場合のチェック
        let var_type = check_expression(env, exp)?;
        if type_ref.type_name() != var_type.type_name() {
            return Err(Error::new(
                &exp.1,
                format!("type error. initialize variable type is {}, value type is {}", type_ref.type_name(), var_type.type_name()),
            ));
        }
        return Ok(type_ref.clone());
    };
    // 左辺の型情報（type_ref）を使って、右辺の初期化子をチェックする
    let loc = &exp.1;
    match type_ref {
        TypeRef::Named(_) => {
            if let Some(ty) = env.find_type(type_ref) {
                check_declarator(env, &ty, decl, external_loc)
            } else {
                Ok(type_ref.clone())
            }
        },
        TypeRef::Pointer(innter_ty) => {
            Err(Error::new(loc, "invalid initializer for pointer type".to_string()))
        },
        TypeRef::Array { type_dec, size } => todo!(),
        TypeRef::Struct { tag_name: _, members: defined_members } => {
            if init_elms.len() != defined_members.len() {
                return Err(Error::new(&loc, format!("initializer length should be {}, but is {}", defined_members.len(), init_elms.len())));
            }
            let errors: Vec<String> = init_elms
                .iter()
                .zip(defined_members)
                .filter_map(|(init_elm, decl)| {
                    match check_expression(env, init_elm) {
                        Ok(init_elm_ty) => {
                            if decl.type_dec != init_elm_ty {
                                Some(build_error_msg(
                                    &init_elm.1,
                                    format!("type mismatched for initializer. left type is {}, right type is {}", decl.type_dec.type_name(), init_elm_ty.type_name())
                                ))
                            } else {
                                None
                            }
                        },
                        Err(e) => e.errors.first().cloned(),
                    }
                }).collect();
            if errors.is_empty() {
                Ok(type_ref.clone())
            } else {
                Err(Error { errors })
            }
        },
    }
}

fn check_basic_calc_operator(env: &Env, left: &ExpressionNode, right: &ExpressionNode) -> Result<TypeRef> {
    let mut errors: Vec<String> = vec![];
    let left_type = check_expression(env, left)?;
    if left_type.type_name() != "int" {
        let (_, loc) = left;
        errors.push(build_error_msg(loc, format!("type error. left should be `int`, but is {}", left_type.type_name())));
    }
    let right_type = check_expression(env,right)?;
    if right_type.type_name() != "int" {
        let (_, loc) = right;
        errors.push(build_error_msg(loc,format!("type error. right should be `int`, but is {}", right_type.type_name())));
    }
    if !errors.is_empty() {
        return Err(Error { errors });
    }
    if left_type.type_name() != right_type.type_name() {
        let (_, loc) = left;
        errors.push(build_error_msg(loc, format!("type error. left type is {}, right type is {}", left_type.type_name(), right_type.type_name())));
        return Err(Error { errors });
    }
    Ok(left_type)
}

fn check_struct(env: &Env, left_type: &TypeRef, left_loc: &Loc, right: &ExpressionNode) -> Result<TypeRef> {
    // ".", "->" の左側オペランドが、structの定義に含まれているかチェック
    let struct_defined = env.find_type(&left_type);
    struct_defined
        .ok_or(Error::new(left_loc, format!("struct `{}` is not defined", left_type.type_name())))
        .and_then(|def| {
            let TypeRef::Struct { tag_name: _, members: defined_members } = def else {
                return Err(Error::new(left_loc, format!("left type is not struct. left type is {:?}", left_type)));
            };
            // ".", "->" の右側オペランドが、structのフィールド定義に含まれているかチェック
            let (Expression::Identifier(operand_member), _) = right else {
                return Err(Error::new(&right.1, format!("right is not Identifier. right: {:?}", right.0)));
            };
            if let Some(defined) = defined_members.iter().find(|m| m.name == operand_member.to_string()) {
                env.find_type(&defined.type_dec)
                    .ok_or(Error::new(&right.1, format!("field `{}` type is not defined", defined.type_dec.type_name())))
            } else {
                Err(Error::new(
                    &right.1,
                    format!(
                        "field `{}` is not defined. defined members: {{{}}}",
                        operand_member,
                        defined_members.into_iter().map(|m| m.name.to_string()).collect::<Vec<String>>().join(", ")
                    ),
                ))
            }
        })
}

fn generate_c_types() -> HashSet<TypeRef> {
    let mut types = HashSet::new();

    types.insert(TypeRef::Named("void".to_string()));
    types.insert(TypeRef::Named("int".to_string()));
    types.insert(TypeRef::Named("char".to_string()));

    // todo...

    types
}


#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn test_type_error_at_check_declarator() {
        // given
        let input = "
int x = 'a';
int y = 1 + 'a';
char c = 1 > 2;
int cond = 1 <= 'a';

int inc(int a) {
    int b = a + 1;
    return b + c;
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert_eq!(errors.len(), 5);
            assert_eq!(true, errors[0].starts_with("error:2:9: type error. initialize variable type is"), "actual message: `{}`", errors[0]);
            assert_eq!(true, errors[1].starts_with("error:3:13: type error. right should be `int`, but is"), "actual message: `{}`", errors[1]);
            assert_eq!(true, errors[2].starts_with("error:4:12: type error. initialize variable type is"), "actual message: `{}`", errors[2]);
            assert_eq!(true, errors[3].starts_with("error:5:17: type error. right should be `int`, but is"), "actual message: `{}`", errors[3]);
            assert_eq!(true, errors[4].starts_with("error:9:16: variable `c` is not defined"), "actual message: `{}`", errors[4]);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_type_error_at_if_condition() {
        // given
        let input = "
int main() {
    if ('a') {
        int y = 1 + 'a';
    } else {
        int y = 1;
    }
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert_eq!(errors.len(), 2);
            assert_eq!(true, errors[0].starts_with("error:3:9: type error. condition type is"), "actual message: `{}`", errors[0]);
            assert_eq!(true, errors[1].starts_with("error:4:21: type error. right should be `int`, but is"), "actual message: `{}`", errors[1]);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_type_error_at_function_call() {
        // given
        let input = "
int inc(int a) {
    return a + 1;
}
int main() {
    int x1 = 1;
    int x2 = random();

    char y1 = inc(x1);
    char y2;
    y2 = inc(x1);

    int z1 = inc();
    int z2 = inc('a');
    int z3 = inc(x1, x1);
    return 0;
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert_eq!(errors.len(), 6);
            assert_eq!("error:7:20: function `random` is not defined", errors[0]);
            assert_eq!("error:9:18: type error. initialize variable type is char, value type is int", errors[1]);
            assert_eq!("error:11:5: type mismatched for operator `=`. left type is char, right type is int", errors[2]);
            assert_eq!("error:13:17: wrong number of arguments, `inc`.", errors[3]);
            assert_eq!("error:14:18: mismatched type for argument 1 in function call, `inc`.", errors[4]);
            assert_eq!("error:15:17: wrong number of arguments, `inc`.", errors[5]);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_check_valid_struct() {
        // given
        let input = r#"
struct person {
    int age;
    char* name;
};

struct wrapper {
    struct person p;
};

struct wrapper* newWrapper(struct person p) {
    struct wrapper w;
    w.p = p;
    return &w;
}

int main() {
    struct person p;
    p.age = 20;

    (&p)->name = "Taro";

    char* s = "Jiro";
    (*(&p)).name = s;

    struct wrapper* w = newWrapper(p);

    struct person p2 = { 30, "Dave" };
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert!(false, "errors: {:?}", errors);
        } else {
            assert!(true);
        }
    }

    #[test]
    fn test_check_invalid_struct() {
        // given
        let input = r#"
struct person {
    int age;
    char* name;
};

int main() {
    struct person p;
    p.age = "foo";
    p.agee = 10;

    (&p)->name = 10;
    (&p)->namee = "Taro";
    p->name = "Taro";

    char* s = "Jiro";
    (*(&p)).name = 10;
    (*(&p)).namee = s;
    (&p).name = s;

    pp.age = 10;
    p.1 = 10;

    struct person p2 = { "Dave", 30 };
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert_eq!(errors.len(), 12);
            assert_eq!("error:9:6: type mismatched for operator `=`. left type is int, right type is char*", errors[0]);
            assert_eq!("error:10:7: field `agee` is not defined. defined members: {age, name}", errors[1]);
            assert_eq!("error:12:9: type mismatched for operator `=`. left type is char*, right type is int", errors[2]);
            assert_eq!("error:13:11: field `namee` is not defined. defined members: {age, name}", errors[3]);
            assert_eq!("error:14:5: left operand is not pointer. left is `struct person`.", errors[4]);
            assert_eq!("error:17:12: type mismatched for operator `=`. left type is char*, right type is int", errors[5]);
            assert_eq!("error:18:13: field `namee` is not defined. defined members: {age, name}", errors[6]);
            assert_eq!("error:19:6: left type is not struct. left type is Pointer(Struct { tag_name: Some(\"person\"), members: [StructDecl { type_dec: Named(\"int\"), name: \"age\" }, StructDecl { type_dec: Pointer(Named(\"char\")), name: \"name\" }] })", errors[7]);
            assert_eq!("error:21:5: variable `pp` is not defined", errors[8]);
            assert_eq!("error:22:7: right is not Identifier. right: Int(1)", errors[9]);
            assert_eq!("error:24:26: type mismatched for initializer. left type is int, right type is char*", errors[10]);
            assert_eq!("error:24:34: type mismatched for initializer. left type is char*, right type is int", errors[11]);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_check_valid_array() {
        // given
        let input = r#"
int main() {
    int xs[10];
    xs[0] = 2;

    int i = 1;
    xs[i] = 5;

    int* ip = &i;
    xs[*ip] = 5;

    int* ys;
    ys[0] = 2;
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert!(false, "errors: {:?}", errors);
        } else {
            assert!(true);
        }
    }

    #[test]
    fn test_check_invalid_array() {
        // given
        let input = r#"
int main() {
    int xs[10];
    xs[0] = "foo";
    xs["a"] = 3;

    int a;
    a[0] = 2;

    int i = 1;
    xs[&i] = 5;

    int* ip = &i;
    xs[ip] = 5;

    int* ys;
    ys[0] = "foo";
    ys["a"] = 3;
    ys[&i] = 5;
    ys[ip] = 5;
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert_eq!(errors.len(), 9);
            assert_eq!("error:4:7: type mismatched for operator `=`. left type is int, right type is char*", errors[0]);
            assert_eq!("error:5:5: index should be int. but it is `char*`.", errors[1]);
            assert_eq!("error:8:5: index operand should be array. but it is `int`.", errors[2]);
            assert_eq!("error:11:5: index should be int. but it is `int*`.", errors[3]);
            assert_eq!("error:14:5: index should be int. but it is `int*`.", errors[4]);
            assert_eq!("error:17:7: type mismatched for operator `=`. left type is int, right type is char*", errors[5]);
            assert_eq!("error:18:5: index should be int. but it is `char*`.", errors[6]);
            assert_eq!("error:19:5: index should be int. but it is `int*`.", errors[7]);
            assert_eq!("error:20:5: index should be int. but it is `int*`.", errors[8]);
        } else {
            assert!(false);
        }
    }
}
