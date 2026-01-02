use std::fmt;

use crate::{parser::ast::{
    Expression, ExpressionNode, ExternalItem, FunctionDecl, Loc, Parameter, Program, Statement, StatementNode, StructDecl, StructRef, TypeRef
}, sema::env::{Env, Functions, LocalScope}};

#[derive(Debug)]
pub struct ScopeError {
    errors: Vec<String>,
}

impl ScopeError {
    fn new(loc: &Loc, msg: String) -> ScopeError {
        ScopeError {
            errors: vec![build_error_msg(loc, msg)],
        }
    }
}

fn build_error_msg(loc: &Loc, msg: String) -> String {
    format!("error:{}:{}: {}", loc.row, loc.col, msg)
}

impl fmt::Display for ScopeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl core::error::Error for ScopeError {}

pub type Result<T> = std::result::Result<T, ScopeError>;

pub fn check_scope(ast: &Program) -> Result<()> {
    let mut results: Vec<ScopeError> = vec![];
    let mut env = Env::new();

    for item_node in &ast.external_item_nodes {
        let (item, external_item_loc) = item_node;
        match item {
            ExternalItem::StructDeclNode(struct_decl) => {
                let StructDecl {
                    tag_name: _,
                    members,
                } = struct_decl;
                let es: Vec<ScopeError> = members
                    .iter()
                    .filter_map(|member| {
                        env.solve_type(&member.type_ref)
                            .map_err(|e| ScopeError::new(external_item_loc, e))
                            .err()
                    })
                    .collect();
                if !es.is_empty() {
                    results.extend(es);
                } else {
                    env.put_struct_type(struct_decl.clone());
                }
            }
            ExternalItem::VarDeclNode(items) => {
                for (type_ref, decl) in items {
                    if let std::result::Result::Err(e) = env.solve_type(type_ref) {
                        results.push(ScopeError::new(external_item_loc, e));
                    } else {
                        if let TypeRef::Struct(StructRef::Decl(struct_decl)) = type_ref.clone() {
                            env.put_struct_type(struct_decl);
                        }
                        env.put_vardecl(decl.name.as_str(), type_ref.clone());
                    }
                }
            }
            ExternalItem::FunctionDeclNode(function) => {
                let FunctionDecl {
                    return_type_ref,
                    name,
                    parameters,
                    body: _,
                } = function;
                if let std::result::Result::Err(e) = env.solve_type(return_type_ref) {
                    results.push(ScopeError::new(external_item_loc, e));
                }
                for p in parameters {
                    if let std::result::Result::Err(e) = env.solve_type(&p.type_ref) {
                        results.push(ScopeError::new(external_item_loc, e));
                    }
                }
                env.put_function(function.clone());
            }
            _ => {}
        }
    }

    println!("=================@@@@ check_function env: {:?}", &env);

    let results: Vec<ScopeError> = ast
        .external_item_nodes
        .iter()
        .filter_map(|node| {
            let (item, _loc) = node;
            if let ExternalItem::FunctionDeclNode(FunctionDecl {
                return_type_ref: _,
                name,
                parameters,
                body,
            }) = item {
                let fs = &env.functions.clone();
                let s = &env.scope(0).clone();
                Some(check_function(
                    &mut env,
                    fs,
                    s,
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
    Err(ScopeError {
        errors: results.iter().flat_map(|e| e.errors.clone()).collect(),
    })
}

fn check_function(
    mut env: &mut Env,
    functions: &Functions,
    global_scope: &LocalScope,
    name: &str,
    parameters: &Vec<Parameter>,
    body: &Option<Box<StatementNode>>,
) -> Vec<ScopeError> {
    let mut results: Vec<ScopeError> = vec![];

    // check parameters
    env = env.create_scope(global_scope.clone());
    let mut local_scope = env.pop_scope();
    for p in parameters {
        if local_scope.find(env, name).is_some() {
            return vec![ScopeError {
                errors: vec![format!("parameter `{}` is duplicated", name)],
            }];
        }
        local_scope.put(env, p.name.as_str(), p.type_ref.clone());
    }

    // check function body
    if let Some(stmt) = body {
        let mut es: Vec<ScopeError> = check_statement(env, functions, &mut local_scope, stmt)
            .into_iter()
            .filter_map(|a| a.err())
            .collect();
        results.append(&mut es);
    }
    results
}

fn check_statement(
    mut env: &mut Env,
    functions: &Functions,
    scope: &mut LocalScope,
    stmt_node: &StatementNode,
) -> Vec<Result<()>> {
    println!("@@@@ check_statement ::: local_scope: {:?}", scope);
    let (stmt, _) = stmt_node;
    match stmt {
        Statement::Return(expression) => {
            if let Some(exp) = expression {
                vec![check_expression(env, functions, scope, exp)]
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
            for (type_ref, decl) in items {
                if scope.find(env, decl.name.as_str()).is_some() {
                    results.push(Err(ScopeError {
                        errors: vec![format!("variable `{}` is duplicated", decl.name)],
                    }));
                } else {
                    scope.put(env, decl.name.as_str(), type_ref.clone());
                }
                decl.value.iter().for_each(|exp| {
                    results.push(check_expression(env, functions, scope, exp));
                });
            }
            results
        }
        Statement::Block(statements) => {
            env = env.create_scope(scope.clone());
            let mut local_scope = env.pop_scope();
            statements
                .iter()
                .flat_map(|stmt| check_statement(env, functions, &mut local_scope, stmt))
                .collect()
        }
        Statement::If {
            condition,
            consequence,
            alternative,
        } => {
            let mut results: Vec<Result<()>> = vec![];
            let condition_result = check_expression(env, functions, scope, condition);
            let mut consequence_result = check_statement(env, functions, scope, consequence);
            let mut alternative_result = alternative
                .as_ref()
                .map(|stmt| check_statement(env, functions, scope, stmt))
                .unwrap_or(vec![]);
            results.push(condition_result);
            results.append(&mut consequence_result);
            results.append(&mut alternative_result);
            results
        }
        Statement::Switch {
            condition,
            switch_block,
        } => {
            let mut results: Vec<Result<()>> = vec![];
            results.push(check_expression(env, functions, scope, condition));
            for stmt in &switch_block.body {
                results.append(&mut check_statement(env, functions, scope, stmt));
            }
            results
        }
        Statement::While { condition, body } => {
            let mut results: Vec<Result<()>> = vec![];
            results.push(check_expression(env, functions, scope, condition));
            results.append(&mut check_statement(env, functions, scope, body));
            results
        }
        Statement::DoWhile { body, condition } => {
            let mut results: Vec<Result<()>> = vec![];
            results.append(&mut check_statement(env, functions, scope, body));
            results.push(check_expression(env, functions, scope, condition));
            results
        }
        Statement::For {
            init,
            condition,
            post,
            body,
        } => {
            let mut results: Vec<Result<()>> = vec![];
            init.iter().for_each(|init_stmt| {
                results.push(check_expression(env, functions, scope, init_stmt));
            });
            condition.iter().for_each(|condition_stmt| {
                results.push(check_expression(env, functions, scope, condition_stmt));
            });
            post.iter().for_each(|post_stmt| {
                results.push(check_expression(env, functions, scope, post_stmt));
            });
            results.append(&mut check_statement(env, functions, scope, body));
            results
        }
        Statement::ExpressionStatement(expression) => {
            vec![check_expression(env, functions, scope, expression)]
        }
    }
}

fn check_expression(
    env: &Env,
    functions: &Functions,
    scope: &LocalScope,
    exp_node: &ExpressionNode,
) -> Result<()> {
    let (exp, _) = exp_node;
    match exp {
        Expression::Int(_) => Ok(()),
        Expression::CharacterLiteral(_) => Ok(()),
        Expression::StringLiteral(_) => Ok(()),
        Expression::Identifier(name) => {
            if !scope.find(env, name).is_some() {
                Err(ScopeError {
                    errors: vec![format!("variable `{}` is not defined", name)],
                })
            } else {
                Ok(())
            }
        }
        Expression::Prefix { operator: _, right } => check_expression(env, functions, scope, right),
        Expression::Infix {
            operator,
            left,
            right,
        } => {
            check_expression(env, functions, scope, left)?;

            // 構造体のメンバーのチェックはscope_checkerの対象外とする
            if operator == "." || operator == "->" {
                return Ok(());
            }

            check_expression(env, functions, scope, right)
        }
        Expression::Postfix { operator: _, left } => check_expression(env, functions, scope, left),
        Expression::FunctionCall {
            function_name,
            arguments,
        } => {
            if !functions.find(function_name).is_some() {
                return Err(ScopeError {
                    errors: vec![format!("function `{}` is not defined", function_name)],
                });
            }
            for arg in arguments.iter() {
                check_expression(env, functions, scope, arg)?;
            }
            Ok(())
        }
        Expression::Initializer { elements } => {
            for element in elements.iter() {
                check_expression(env, functions, scope, element)?;
            }
            Ok(())
        }
        Expression::Index { left, index } => {
            check_expression(env, functions, scope, left)?;
            check_expression(env, functions, scope, index)
        }
    }
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
        if let Some(ScopeError { errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(
                "variable `p` is duplicated",
                errors[0]
            );
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
        if let Some(ScopeError { errors }) = result.err() {
            assert_eq!(errors.len(), 15);
            assert_eq!(
                "variable `a` is not defined",
                errors[0]
            );
            assert_eq!(
                "variable `foo` is not defined",
                errors[1]
            );
            assert_eq!(
                "variable `bar` is not defined",
                errors[2]
            );
            assert_eq!(
                "variable `b` is not defined",
                errors[3]
            );
            assert_eq!(
                "variable `c` is not defined",
                errors[4]
            );
            assert_eq!(
                "variable `zz` is not defined",
                errors[5]
            );
            assert_eq!(
                "variable `yy` is not defined",
                errors[6]
            );
            assert_eq!(
                "variable `m` is not defined",
                errors[7]
            );
            assert_eq!(
                "variable `i` is not defined",
                errors[8]
            );
            assert_eq!(
                "variable `j` is not defined",
                errors[9]
            );
            assert_eq!(
                "variable `k` is not defined",
                errors[10]
            );
            assert_eq!(
                "variable `xx` is not defined",
                errors[11]
            );
            assert_eq!(
                "variable `q` is not defined",
                errors[12]
            );
            assert_eq!(
                "variable `bs` is not defined",
                errors[13]
            );
            assert_eq!(
                "variable `abc` is not defined",
                errors[14]
            );
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
        if let Some(ScopeError { errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(
                "function `bar` is not defined",
                errors[0]
            );
        } else {
            assert!(false);
        }
    }
}
