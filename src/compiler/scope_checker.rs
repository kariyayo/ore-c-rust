use std::collections::HashSet;
use std::fmt;

use crate::parser::ast::{Expression, ExternalItem, Function, Parameter, Program, Statement, TypeRef};

#[derive(Debug)]
pub struct Error {
    errors: Vec<String>,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl core::error::Error for Error {}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
struct Types {
    entities: HashSet<String>,
}

impl Types {
    fn put(&mut self, name: &str) {
        self.entities.insert(name.to_string());
    }
    fn find(&self, name: &str) -> bool {
        self.entities.contains(name)
    }
}

#[derive(Debug)]
struct Functions {
    entities: HashSet<String>,
}

impl Functions {
    fn put(&mut self, name: &str) {
        self.entities.insert(name.to_string());
    }
    fn find(&self, name: &str) -> bool {
        self.entities.contains(name)
    }
}

#[derive(Debug)]
struct LocalScope<'a> {
    parent: Option<&'a LocalScope<'a>>,
    entities: HashSet<String>,
}

impl <'a> LocalScope<'a> {
    fn put(&mut self, name: &str) {
        self.entities.insert(name.to_string());
    }

    fn find(&self, name: &str) -> bool {
        let entity = self.entities.contains(name);
        if entity {
            return true;
        }
        self.parent.as_ref().map(|x| x.find(name)).unwrap_or(false)
    }
}

pub fn check_scope(ast: &Program) -> Result<()> {
    let mut types = Types { entities: generate_c_types() };
    let mut functions = Functions { entities: HashSet::new() };
    let mut global_scope = LocalScope { parent: None, entities: HashSet::new() };

    for item in &ast.external_items {
        match item {
            ExternalItem::Struct(TypeRef::Struct { tag_name: Some(tag_name), members }) => {
                let s = format!("struct {}", tag_name);
                types.put(s.as_str());
            },
            ExternalItem::VarDecl(declarators) => {
                for (_, declarator) in declarators {
                    global_scope.put(declarator.name.as_str());
                    // var_decl_initializer.push(declarator);
                }
            },
            ExternalItem::FunctionDecl(function) => {
                functions.put(&function.name);
            },
            _ => {},
        }
    }

    println!("@@@@ check_function types: {:?}, functions: {:?}, global_scope: {:?}", types, functions, &global_scope);

    let results: Vec<Error> = ast.external_items.iter()
        .filter_map(|item| {
            if let ExternalItem::FunctionDecl(Function { return_type_dec, name, parameters, body }) = item {
                Some(check_function(
                    // &types,
                    &functions,
                    &global_scope,
                    return_type_dec,
                    name,
                    parameters,
                    body,
                ))
            } else {
                None
            }
        })
        .flatten()
        .collect();
    if results.is_empty() {
        return Ok(());
    }
    Err(Error { errors: results.iter().flat_map(|e| e.errors.clone()).collect() })
}

fn check_function(
    // types: &Types,
    functions: &Functions,
    global_scope: &LocalScope,
    return_type_dec: &TypeRef,
    name: &str,
    parameters: &Vec<Parameter>,
    body: &Option<Box<Statement>>,
) -> Vec<Error> {
    let mut results: Vec<Error> = vec![];
    println!("@@@@ check_function ::: return_type_dec: {:?}, name: {:?}, parameters: {:?}", return_type_dec.type_name(), name, parameters);

    // スコープチェックでは型の確認は不要
    // // check return type
    // if !types.find(&return_type_dec.type_name()) {
    //     results.push(Error { errors: vec![format!("return type is not defined: {:?}", return_type_dec.type_name())] });
    //     return results;
    // }

    // check parameters
    let mut local_scope = LocalScope { parent: Some(&global_scope), entities: HashSet::new() };
    for p in parameters {
        if local_scope.find(name) {
            return vec![Error{ errors: vec![format!("parameter `{}` is duplicated", name)] }]
        }
        local_scope.put(p.name.as_str());
    }

    // check function body
    if let Some(stmt) = body {
        let mut es: Vec<Error> = check_statement(functions, &mut local_scope, stmt).into_iter()
            .filter_map(|a| a.err())
            .collect();
        results.append(&mut es);
    }
    results
}

fn check_statement(
    // types: &Types,
    functions: &Functions,
    scope: &mut LocalScope,
    stmt: &Statement,
) -> Vec<Result<()>> {
    println!("@@@@ check_statement ::: local_scope: {:?}", scope);
    match stmt {
        Statement::Return(expression) => {
            if let Some(exp) = expression {
                vec![check_expression(functions, scope, exp)]
            } else {
                vec![Ok(())]
            }
        }
        Statement::Break => {
            vec![Ok(())]
        }
        Statement::Continue => {
            vec![Ok(())]
        }
        Statement::VarDecl(items) => {
            let mut results: Vec<Result<()>> = vec![];
            for (_, decl) in items {
                if scope.find(decl.name.as_str()) {
                    results.push(Err(Error { errors: vec![format!("variable `{}` is duplicated", decl.name)] }));
                } else {
                    scope.put(decl.name.as_str());
                }
                decl.value.iter().for_each(|exp| {
                    results.push(check_expression(functions, scope, exp));
                });
            }
            results
        },
        Statement::Block(statements) => {
            let mut local_scope = LocalScope { parent: Some(&scope), entities: HashSet::new() };
            statements.iter().map(|stmt| check_statement(functions, &mut local_scope, stmt)).flatten().collect()
        },
        Statement::If { condition, consequence, alternative } => {
            let mut results: Vec<Result<()>> = vec![];
            let condition_result = check_expression(functions, scope, condition);
            let mut consequence_result = check_statement(functions, scope, consequence);
            let mut alternative_result = alternative
                .as_ref()
                .map(|stmt| check_statement(functions, scope, &stmt))
                .unwrap_or(vec![]);
            results.push(condition_result);
            results.append(&mut consequence_result);
            results.append(&mut alternative_result);
            results
        },
        Statement::Switch { condition, switch_block } => {
            let mut results: Vec<Result<()>> = vec![];
            results.push(check_expression(functions, scope, condition));
            for stmt in &switch_block.body {
                results.append(&mut check_statement(functions, scope, &stmt));
            }
            results
        },
        Statement::While { condition, body } => {
            let mut results: Vec<Result<()>> = vec![];
            results.push(check_expression(functions, scope, condition));
            results.append(&mut check_statement(functions, scope, body));
            results
        },
        Statement::DoWhile { body, condition } => {
            let mut results: Vec<Result<()>> = vec![];
            results.append(&mut check_statement(functions, scope, body));
            results.push(check_expression(functions, scope, condition));
            results
        },
        Statement::For { init, condition, post, body } => {
            let mut results: Vec<Result<()>> = vec![];
            init.iter().for_each(|init_stmt| {
                results.push(check_expression(functions, scope, init_stmt));
            });
            condition.iter().for_each(|condition_stmt| {
                results.push(check_expression(functions, scope, condition_stmt));
            });
            post.iter().for_each(|post_stmt| {
                results.push(check_expression(functions, scope, post_stmt));
            });
            results.append(&mut check_statement(functions, scope, body));
            results
        },
        Statement::ExpressionStatement(expression) => vec![check_expression(functions, scope, expression)],
    }
}

fn check_expression(
    functions: &Functions,
    scope: &LocalScope,
    exp: &Expression,
) -> Result<()> {
    match exp {
        Expression::Int(_) => Ok(()),
        Expression::CharacterLiteral(_) => Ok(()),
        Expression::StringLiteral(_) => Ok(()),
        Expression::Identifier(name) => {
            if !scope.find(name) {
                Err(Error { errors: vec![format!("variable `{}` is not defined", name)] })
            } else {
                Ok(())
            }
        },
        Expression::PrefixExpression { operator: _, right } => check_expression(functions, scope, right),
        Expression::InfixExpression { operator, left, right } => {
            check_expression(functions, scope, left)?;

            // 構造体のメンバーのチェックはscope_checkerの対象外とする
            if operator == "." || operator == "->" {
                return Ok(());
            }

            check_expression(functions, scope, right)
        },
        Expression::PostfixExpression { operator: _, left } => check_expression(functions, scope, left),
        Expression::FunctionCallExpression { function_name, arguments } => {
            if !functions.find(function_name) {
                return Err(Error { errors: vec![format!("function `{}` is not defined", function_name)] });
            }
            for arg in arguments {
                check_expression(functions, scope, arg)?;
            }
            Ok(())
        },
        Expression::InitializerExpression { elements } => {
            for element in elements {
                check_expression(functions, scope, element)?;
            }
            Ok(())
        },
        Expression::IndexExpression { left, index } => {
            check_expression(functions, scope, left)?;
            check_expression(functions, scope, index)
        },
    }
}

fn generate_c_types() -> HashSet<String> {
    let mut types = HashSet::new();

    types.insert("void".to_string());
    types.insert("int".to_string());
    types.insert("char".to_string());

    // todo...

    types
}


#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    #[test]
    fn test_variable_duplicated() {
        // given
        let input = "
int x = 10;
struct point {
    int x;
    int y;
};
struct rect foo(point p) {
    int p = 1; // duplicated
    struct point pp = {p.x + 10, p.y + 10};
    return pp;
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_scope(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(true, errors[0].starts_with("variable `p` is duplicated"), "actual message: `{}`", errors[0]);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_not_defined() {
        // given
        let input = "
int x = 10;
struct point {
    int x;
    int y;
};
struct point foo(point p) {
    struct point pp = {p.x + 10, p.y + 10};
    if (a == 0) {
        ++(foo.x);
    } else {
        (bar.x)--;
    }
    switch (p.x) {
        case 1:
        case 2:
            return b;
        default:
            return c;
    }
    while (p.x > p.y) {
        zz++;
    }
    do {
        yy++;
    } while (m > n);
    for (i = 0; j < l; k++) {
        xx++;
    }
    int as[] = {q, r};
    bs[k];
    foo(abc);
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_scope(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert_eq!(errors.len(), 15);
            assert_eq!(true, errors[0].starts_with("variable `a` is not defined"), "actual message: `{}`", errors[0]);
            assert_eq!(true, errors[1].starts_with("variable `foo` is not defined"), "actual message: `{}`", errors[1]);
            assert_eq!(true, errors[2].starts_with("variable `bar` is not defined"), "actual message: `{}`", errors[2]);
            assert_eq!(true, errors[3].starts_with("variable `b` is not defined"), "actual message: `{}`", errors[3]);
            assert_eq!(true, errors[4].starts_with("variable `c` is not defined"), "actual message: `{}`", errors[4]);
            assert_eq!(true, errors[5].starts_with("variable `zz` is not defined"), "actual message: `{}`", errors[5]);
            assert_eq!(true, errors[6].starts_with("variable `yy` is not defined"), "actual message: `{}`", errors[6]);
            assert_eq!(true, errors[7].starts_with("variable `m` is not defined"), "actual message: `{}`", errors[7]);
            assert_eq!(true, errors[8].starts_with("variable `i` is not defined"), "actual message: `{}`", errors[8]);
            assert_eq!(true, errors[9].starts_with("variable `j` is not defined"), "actual message: `{}`", errors[9]);
            assert_eq!(true, errors[10].starts_with("variable `k` is not defined"), "actual message: `{}`", errors[10]);
            assert_eq!(true, errors[11].starts_with("variable `xx` is not defined"), "actual message: `{}`", errors[11]);
            assert_eq!(true, errors[12].starts_with("variable `q` is not defined"), "actual message: `{}`", errors[12]);
            assert_eq!(true, errors[13].starts_with("variable `bs` is not defined"), "actual message: `{}`", errors[13]);
            assert_eq!(true, errors[14].starts_with("variable `abc` is not defined"), "actual message: `{}`", errors[14]);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_not_defined_function() {
        // given
        let input = "
int x = 10;
struct point {
    int x;
    int y;
};
struct point foo(point p) {
    bar(p);
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_scope(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(true, errors[0].starts_with("function `bar` is not defined"), "actual message: `{}`", errors[0]);
        } else {
            assert!(false);
        }
    }
}
