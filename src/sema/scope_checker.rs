use std::collections::HashMap;
use std::fmt;

use crate::{parser::ast::{
    Expression, ExpressionNode, ExternalItem, FunctionDecl, Loc, Parameter, Program, Statement, StatementNode, StructDecl, StructRef, TypeRef
}, sema::env::{Env, Functions, LocalScope, Scope, put_typedef}};

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

pub fn check_scope<'a>(env: &'a mut Env, ast: &Program) -> Result<&'a mut Env> {
    let mut functions = Functions { entities: HashMap::new() };
    let mut global_scope = env.create_global_scope();

    for item_node in &ast.external_item_nodes {
        let (item, _) = item_node;
        match item {
            ExternalItem::StructDeclNode(struct_decl) => {
                global_scope.put_struct_type(struct_decl.clone());
            }
            ExternalItem::VarDeclNode(declarators) => {
                for (type_ref, declarator) in declarators {
                    println!("=>>> BEFORE put_var from ExternalItem::VarDeclNode");
                    global_scope.put_var(declarator.name.as_str(), type_ref);
                }
            }
            ExternalItem::FunctionDeclNode(function) => {
                functions.put(function.clone());
            }
            ExternalItem::TypedefNode(type_ref, items) => {
                put_typedef(env, &mut global_scope, type_ref, items);
            },
        }
    }
    let scope = Scope::create_global_scope(global_scope);
    println!("0000000 global_scope: {:?}", scope);
    env.set_global_scope(scope);
    env.put_functions(functions);

    let results: Vec<ScopeError> = ast
        .external_item_nodes
        .iter()
        .filter_map(|(item, loc)| {
            if let ExternalItem::FunctionDeclNode(FunctionDecl {
                return_type_ref: _,
                name,
                parameters,
                body,
            }) = item
            {
                Some(check_function(
                    env,
                    loc,
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
    // println!("******** BEFORE return, {:?}", env.scopes);
    if results.is_empty() {
        return Ok(env);
    }
    Err(ScopeError {
        errors: results.iter().flat_map(|e| e.errors.clone()).collect(),
    })
}

fn check_function(
    env: &mut Env,
    function_loc: &Loc,
    _name: &str,
    parameters: &Vec<Parameter>,
    body: &Option<Box<StatementNode>>,
) -> Vec<ScopeError> {
    let mut results: Vec<ScopeError> = vec![];

    // check parameters
    for p in parameters {
        if env.solve_type_in_global_scope(&p.type_ref).is_none() {
            return vec![ScopeError::new(
                function_loc,
                format!("parameter `{}` is undefined", p.type_ref.type_name()),
            )]
        }
    }

    // check function body
    let global_scope_id = 0;
    if let Some(stmt) = body {
        let mut local_scope = env.create_local_scope(global_scope_id);
        for p in parameters {
            println!("=>>> BEFORE put_var from `check_function`");
            local_scope.put_var(p.name.as_str(), &p.type_ref);
        }
        println!("AFTER put_var from `check_function`. local_scope: {:?}", local_scope);
        let (s, res) = check_statement(env, local_scope, stmt);
        println!("AFTER check_statement from `check_function`. s: {:?}", s);
        for r in res {
            if let Some(e) = r.err() {
                results.push(e);
            }
        }


        env.put_scope(*stmt.clone(), s.fix());
        // if env.scope_by_node(stmt).is_none() {
        //     // println!("******* ===== {:?}", s);
        //     // println!("BEFORE append_scope in `check_function");
        //     env.append_scope(*stmt.clone(), s.fix());
        // }
    }
    results
}

fn check_statement<'a>(
    env: &mut Env,
    // functions: &Functions,
    mut local_scope: LocalScope,
    stmt_node: &StatementNode,
) -> (LocalScope, Vec<Result<()>>) {
    // println!("****** ````` START {:?}", local_scope);
    let (stmt, loc) = stmt_node;
    let res = match stmt {
        Statement::Return(expression) => {
            if let Some(exp) = expression {
                let res = vec![check_expression(env, &local_scope, exp)];
                res
                // vec![check_expression(functions, scope, exp)]
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
        Statement::Typedef(type_ref, items) => {
            let mut results: Vec<Result<()>> = vec![];
            put_typedef(env, &mut local_scope, type_ref, items);
            results
        }
        Statement::VarDecl(items) => {
            let mut results: Vec<Result<()>> = vec![];
            for (type_ref, decl) in items {
                if env.solve_type_from_local_scope(&local_scope, type_ref).is_none() {
                    results.push(Err(ScopeError::new(
                        loc,
                        format!("type `{}` is undefined", type_ref.type_name()),
                    )));
                    continue;
                }
                if env.is_defined(&local_scope, decl.name.as_str()) {
                    results.push(Err(ScopeError::new(
                        loc,
                        format!("variable `{}` is duplicated", decl.name),
                    )));
                } else {
                    println!("=>>> BEFORE put_var from `check_statement`");
                    local_scope.put_var(decl.name.as_str(), type_ref);
                }
                decl.value.iter().for_each(|exp| {
                    results.push(check_expression(env, &local_scope, exp));
                });
            }
            results
        }
        Statement::Block(statements) => {
            let scope = local_scope.fix();
            env.put_scope(stmt_node.clone(), scope);
            // println!("BEFORE append_scope in `check_statement` for local_scope");
            // env.append_scope(stmt_node.clone(), scope);

            // Statement::Block内のスコープ
            let mut block_scope = env.create_local_scope(local_scope.id);
            let mut results: Vec<Result<()>> = vec![];
            for stmt in statements {
                let (ns, res) = check_statement(env, block_scope, stmt);
                // println!("++++++++++++++ {:?}", ns);
                block_scope = ns;
                results.extend(res);
            }


            let s = block_scope.fix();
            env.put_scope(stmt_node.clone(), s);
            // if env.scope_by_node(stmt_node).is_none() {
            //     let s = block_scope.fix();
            //     // println!("BEFORE append_scope in `check_statement` for new_scope");
            //     env.append_scope(stmt_node.clone(), s);
            // }
            results
        }
        Statement::If {
            condition,
            consequence,
            alternative,
        } => {
            let mut results: Vec<Result<()>> = vec![];
            let condition_result = check_expression(env, &local_scope, condition);
            results.push(condition_result);
            let (s, mut consequence_result) = check_statement(env, local_scope, consequence);
            local_scope = s;
            results.append(&mut consequence_result);
            if let Some(stmt_node) = alternative {
                let (s, mut alternative_result) = check_statement(env, local_scope, stmt_node);
                local_scope = s;
                results.append(&mut alternative_result);
            }
            results
        }
        Statement::Switch {
            condition,
            switch_block,
        } => {
            let mut results: Vec<Result<()>> = vec![];
            results.push(check_expression(env, &local_scope, condition));
            for stmt in &switch_block.body {
                let (s, res) = check_statement(env, local_scope, stmt);
                local_scope = s;
                results.extend(res);
            }
            results
        }
        Statement::While { condition, body } => {
            let mut results: Vec<Result<()>> = vec![];
            results.push(check_expression(env, &local_scope, condition));
            let (s, res) = check_statement(env, local_scope, body);
            local_scope = s;
            results.extend(res);
            results
        }
        Statement::DoWhile { body, condition } => {
            let mut results: Vec<Result<()>> = vec![];
            let (s, res) = check_statement(env, local_scope, body);
            local_scope = s;
            results.extend(res);
            results.push(check_expression(env, &local_scope, condition));
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
                results.push(check_expression(env, &local_scope, init_stmt));
            });
            condition.iter().for_each(|condition_stmt| {
                results.push(check_expression(env, &local_scope, condition_stmt));
            });
            post.iter().for_each(|post_stmt| {
                results.push(check_expression(env, &local_scope, post_stmt));
            });
            let (s, res) = check_statement(env, local_scope, body);
            local_scope = s;
            results.extend(res);
            results
        }
        Statement::ExpressionStatement(expression) => {
            vec![check_expression(env, &local_scope, expression)]
        }
    };
    // println!("****** ````` END {:?}", local_scope);
    (local_scope, res)
}

fn check_expression(
    env: &Env,
    // functions: &Functions,
    local_scope: &LocalScope,
    exp_node: &ExpressionNode,
) -> Result<()> {
    let (exp, loc) = exp_node;
    match exp {
        Expression::Int(_) => Ok(()),
        Expression::CharacterLiteral(_) => Ok(()),
        Expression::StringLiteral(_) => Ok(()),
        Expression::Identifier(name) => {
            if !env.is_defined(local_scope, name) {
                Err(ScopeError::new(
                    loc,
                    format!("variable `{}` is not defined", name),
                ))
            } else {
                Ok(())
            }
        }
        Expression::Prefix { operator: _, right } => check_expression(env, local_scope, right),
        Expression::Infix {
            operator,
            left,
            right,
        } => {
            check_expression(env, local_scope, left)?;

            // 構造体のメンバーのチェックはscope_checkerの対象外とする
            if operator == "." || operator == "->" {
                return Ok(());
            }

            check_expression(env, local_scope, right)
        }
        Expression::Postfix { operator: _, left } => check_expression(env, local_scope, left),
        Expression::FunctionCall {
            function_name,
            arguments,
        } => {
            if env.find_function(function_name.as_str()).is_none() {
                return Err(ScopeError::new(
                    loc,
                    format!("function `{}` is not defined", function_name),
                ));
            }
            for arg in arguments.iter() {
                check_expression(env, local_scope, arg)?;
            }
            Ok(())
        }
        Expression::Initializer { elements } => {
            for element in elements.iter() {
                check_expression(env, local_scope, element)?;
            }
            Ok(())
        }
        Expression::Index { left, index } => {
            check_expression(env, local_scope, left)?;
            check_expression(env, local_scope, index)
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
struct point foo(struct point p) {
    int p = 1; // duplicated
    struct point pp = {p.x + 10, p.y + 10};
    return pp;
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = &mut Env::new();
        let result = check_scope(env, &ast);

        // then
        if let Some(ScopeError { errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(
                errors[0],
                "error:8:5: variable `p` is duplicated",
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
struct point foo(struct point p) {
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
        let env = &mut Env::new();
        let result = check_scope(env, &ast);

        // then
        if let Some(ScopeError { errors }) = result.err() {
            assert_eq!(errors.len(), 15);
            assert_eq!(
                errors[0],
                "error:9:9: variable `a` is not defined",
            );
            assert_eq!(
                errors[1],
                "error:10:12: variable `foo` is not defined",
            );
            assert_eq!(
                errors[2],
                "error:12:10: variable `bar` is not defined",
            );
            assert_eq!(
                errors[3],
                "error:17:20: variable `b` is not defined",
            );
            assert_eq!(
                errors[4],
                "error:19:20: variable `c` is not defined",
            );
            assert_eq!(
                errors[5],
                "error:22:9: variable `zz` is not defined",
            );
            assert_eq!(
                errors[6],
                "error:25:9: variable `yy` is not defined",
            );
            assert_eq!(
                errors[7],
                "error:26:14: variable `m` is not defined",
            );
            assert_eq!(
                errors[8],
                "error:27:10: variable `i` is not defined",
            );
            assert_eq!(
                errors[9],
                "error:27:17: variable `j` is not defined",
            );
            assert_eq!(
                errors[10],
                "error:27:24: variable `k` is not defined",
            );
            assert_eq!(
                errors[11],
                "error:28:9: variable `xx` is not defined",
            );
            assert_eq!(
                errors[12],
                "error:30:17: variable `q` is not defined",
            );
            assert_eq!(
                errors[13],
                "error:31:5: variable `bs` is not defined",
            );
            assert_eq!(
                errors[14],
                "error:32:9: variable `abc` is not defined",
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
struct point foo(struct point p) {
    bar(p);
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = &mut Env::new();
        let result = check_scope(env, &ast);

        // then
        if let Some(ScopeError { errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(
                errors[0],
                "error:8:8: function `bar` is not defined",
            );
        } else {
            assert!(false);
        }
    }
}
