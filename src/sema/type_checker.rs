use std::{ fmt, vec };

use crate::{parser::ast::{
    Declarator, Expression, ExpressionNode, ExternalItem, FunctionDecl, Loc, Parameter, Program,
    Statement, StatementNode, StructDecl, StructRef, SwitchBlock, SwitchLabel, TypeRef,
}, sema::env::{Env, LocalScope, Type}};

#[derive(Debug)]
pub struct TypeError {
    errors: Vec<String>,
}

impl TypeError {
    fn new(loc: &Loc, msg: String) -> TypeError {
        TypeError {
            errors: vec![build_error_msg(loc, msg)],
        }
    }
}

fn build_error_msg(loc: &Loc, msg: String) -> String {
    format!("error:{}:{}: {}", loc.row, loc.col, msg)
}

impl fmt::Display for TypeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for error in &self.errors {
            writeln!(f, "{}", error)?;
        }
        Ok(())
    }
}

impl core::error::Error for TypeError {}

pub type Result<T> = std::result::Result<T, TypeError>;

pub fn check_type(ast: &Program, env: &Env) -> Result<()> {
    let mut results: Vec<TypeError> = vec![];

    for item_node in &ast.external_item_nodes {
        let (item, external_item_loc) = item_node;
        match item {
            ExternalItem::StructDeclNode(struct_decl) => {
                let StructDecl {
                    tag_name: _,
                    members,
                } = struct_decl;
                let es: Vec<TypeError> = members
                    .iter()
                    .filter_map(|member| {
                        env.solve_type(&member.type_ref)
                            .map_err(|e| TypeError::new(external_item_loc, e))
                            .err()
                    })
                    .collect();
                if !es.is_empty() {
                    results.extend(es);
                }
            }
            ExternalItem::VarDeclNode(items) => {
                let global_scope = env.get_global_scope();
                for (type_ref, decl) in items {
                    if let Err(e) = check_declarator(&env, global_scope, type_ref, decl, external_item_loc) {
                        results.push(e);
                    }
                }
            }
            ExternalItem::FunctionDeclNode(function) => {
                let FunctionDecl {
                    return_type_ref: return_type_dec,
                    name: _,
                    parameters,
                    body,
                } = function;
                let global_scope = env.get_global_scope();
                if let Err(e) = check_function_declaration(
                    env,
                    global_scope,
                    return_type_dec,
                    parameters,
                    body,
                    external_item_loc,
                ) {
                    results.push(e);
                }
            }
        }
    }
    if results.is_empty() {
        return Ok(());
    }
    Err(TypeError {
        errors: results.into_iter().flat_map(|e| e.errors).collect(),
    })
}

fn check_statement(env: &Env, scope: &LocalScope, stmt_node: &StatementNode) -> Vec<TypeError> {
    let mut results: Vec<TypeError> = vec![];
    let (stmt, stmt_loc) = stmt_node;
    let scope = env.scope_by_node(scope, stmt_node);
    match stmt {
        Statement::Return(expression) => {
            if let Err(e) = check_return_statement(env, scope, expression) {
                results.push(e);
            }
        }
        Statement::Break | Statement::Continue => {
            // NOP
        }
        Statement::VarDecl(items) => {
            results.append(&mut check_var_decl_statement(env, scope, items, stmt_loc));
        }
        Statement::Block(statements) => {
            results.append(&mut check_block_statement(env, scope, statements));
        }
        Statement::If {
            condition,
            consequence,
            alternative,
        } => {
            results.append(&mut check_if_statement(
                env,
                scope,
                condition,
                consequence,
                alternative,
            ));
        }
        Statement::Switch {
            condition,
            switch_block,
        } => {
            results.append(&mut check_switch_statement(env, scope, condition, switch_block));
        }
        Statement::While { condition, body } => {
            results.append(&mut check_while_statement(env, scope, condition, body));
        }
        Statement::DoWhile { body, condition } => {
            results.append(&mut check_while_statement(env, scope, condition, body));
        }
        Statement::For {
            init,
            condition,
            post,
            body,
        } => {
            results.append(&mut check_for_statement(env, scope, init, condition, post, body));
        }
        Statement::ExpressionStatement(expression) => {
            if let Err(e) = check_expression(env, scope, expression) {
                results.push(e);
            }
        }
    }
    results
}

fn check_return_statement(env: &Env, scope: &LocalScope, expression: &Option<(Expression, Loc)>) -> Result<Type> {
    if let Some(exp) = expression {
        check_expression(env, scope, exp)
    } else {
        Ok(Type::Basic("void".to_string()))
    }
}

fn check_var_decl_statement(
    env: &Env,
    scope: &LocalScope,
    items: &Vec<(TypeRef, Declarator)>,
    loc: &Loc,
) -> Vec<TypeError> {
    let mut errors: Vec<TypeError> = vec![];
    for (type_ref, decl) in items {
        if let Err(e) = check_declarator(env, scope, type_ref, decl, loc) {
            errors.push(e);
        }
    }
    errors
}

fn check_block_statement(env: &Env, scope: &LocalScope, statements: &[StatementNode]) -> Vec<TypeError> {
    let mut errors: Vec<TypeError> = vec![];
    for stmt in statements {
        errors.append(&mut check_statement(env, scope, stmt));
    }
    errors
}

fn check_if_statement(
    env: &Env,
    scope: &LocalScope,
    condition: &(Expression, Loc),
    consequence: &(Statement, Loc),
    alternative: &Option<Box<(Statement, Loc)>>,
) -> Vec<TypeError> {
    let mut errors: Vec<TypeError> = vec![];
    match check_expression(env, scope, condition) {
        Ok(condition_type) => {
            if condition_type.type_name() != "int" {
                errors.push(TypeError::new(
                    &condition.1,
                    format!(
                        "type error. condition type is {}",
                        condition_type.type_name()
                    ),
                ));
            }
            errors.extend(check_statement(env, scope, consequence));
            if let Some(alt_stmt) = alternative {
                errors.extend(check_statement(env, scope, alt_stmt));
            }
        }
        Err(e) => errors.push(e),
    };
    errors
}

fn check_switch_statement(
    env: &Env,
    scope: &LocalScope,
    condition: &(Expression, Loc),
    switch_block: &SwitchBlock,
) -> Vec<TypeError> {
    let mut errors: Vec<TypeError> = vec![];
    match check_expression(env, scope, condition) {
        Ok(condition_type) => {
            match condition_type.type_name().as_str() {
                "int" | "char" | "short" | "long" => {
                    // NOP
                }
                _ => {
                    errors.push(TypeError::new(
                        &condition.1,
                        format!(
                            "type error. switch condition type is {}",
                            condition_type.type_name()
                        ),
                    ));
                    return errors;
                }
            }
            // condition_typeと全てのラベルの型が同じであることをチェック
            switch_block.label_entries.iter().flat_map(|entry| &entry.labels).for_each(|label| {
                if let SwitchLabel::Case(label_exp) = label {
                    match check_expression(env, scope, label_exp) {
                        Ok(label_ty) => {
                            if label_ty != condition_type {
                                errors.push(TypeError::new(
                                    &label_exp.1,
                                    format!(
                                        "type error. switch label type is {}, but switch condition type is {}",
                                        label_ty.type_name(),
                                        condition_type.type_name(),
                                    ),
                                ));
                            }
                        },
                        Err(e) => errors.push(e),
                    }
                }
            });
            // bodyをチェック
            for stmt in &switch_block.body {
                errors.extend(check_statement(env, scope, stmt));
            }
        }
        Err(e) => errors.push(e),
    }
    errors
}

fn check_while_statement(
    env: &Env,
    scope: &LocalScope,
    condition: &(Expression, Loc),
    body: &(Statement, Loc),
) -> Vec<TypeError> {
    let mut errors: Vec<TypeError> = vec![];
    match check_expression(env, scope, condition) {
        Ok(condition_type) => {
            if condition_type.type_name().as_str() != "int" {
                errors.push(TypeError::new(
                    &condition.1,
                    format!(
                        "type error. while condition type is {}",
                        condition_type.type_name()
                    ),
                ));
            }
        }
        Err(e) => errors.push(e),
    }
    errors.extend(check_statement(env, scope, body));
    errors
}

fn check_for_statement(
    env: &Env,
    scope: &LocalScope,
    init: &Option<(Expression, Loc)>,
    condition: &Option<(Expression, Loc)>,
    post: &Option<(Expression, Loc)>,
    body: &(Statement, Loc),
) -> Vec<TypeError> {
    let mut errors: Vec<TypeError> = vec![];
    if let Some(Err(e)) = init.as_ref().map(|exp| check_expression(env, scope, exp)) {
        errors.push(e);
    }
    if let Some(exp) = condition.as_ref() {
        match check_expression(env, scope, exp) {
            Ok(condition_type) => {
                if condition_type.type_name().as_str() != "int" {
                    errors.push(TypeError::new(
                        &exp.1,
                        format!(
                            "type error. for condition type is {}",
                            condition_type.type_name()
                        ),
                    ));
                }
            }
            Err(e) => errors.push(e),
        }
    }
    if let Some(Err(e)) = post.as_ref().map(|exp| check_expression(env, scope, exp)) {
        errors.push(e);
    }
    errors.extend(check_statement(env, scope, body));
    errors
}

fn check_expression(env: &Env, scope: &LocalScope, exp_node: &ExpressionNode) -> Result<Type> {
    let (exp, exp_loc) = exp_node;
    match exp {
        Expression::Int(_) => Ok(Type::Basic("int".to_string())),
        Expression::CharacterLiteral(_) => Ok(Type::Basic("char".to_string())),
        Expression::StringLiteral(_) => {
            Ok(Type::Pointer(Box::new(Type::Basic("char".to_string()))))
        }
        Expression::Identifier(name) => match env.find_vardecl(scope, name) {
            Some(type_ref) => env.solve_type(&type_ref).map_err(|e| TypeError::new(exp_loc, e)),
            None => Err(TypeError::new(
                exp_loc,
                format!("identifier `{}` is not defined", name),
            )),
        },
        Expression::Prefix { operator, right } => match operator.as_str() {
            "!" | "-" | "++" | "--" => {
                let right_type = check_expression(env, scope, right)?;
                if right_type.type_name() != "int" {
                    Err(TypeError::new(
                        &right.1,
                        format!(
                            "type mismatched for prefix operator `{}`. right type is {}",
                            operator,
                            right_type.type_name()
                        ),
                    ))
                } else {
                    Ok(right_type)
                }
            }
            "*" => {
                let right_type = check_expression(env, scope, right)?;
                let Type::Pointer(ty) = right_type else {
                    return Err(TypeError::new(
                        exp_loc,
                        format!(
                            "cannot dereference non-pointer type `{}`",
                            right_type.type_name()
                        ),
                    ));
                };
                Ok(*ty)
            }
            "&" => {
                let right_type = check_expression(env, scope, right)?;
                Ok(Type::Pointer(Box::new(right_type)))
            }
            _ => panic!(
                "prefix is not supported. prefix is {}, right is {:?}",
                operator, right
            ),
        },
        Expression::Infix {
            operator,
            left,
            right,
        } => match operator.as_str() {
            "+" | "-" | "*" | "/" | "%" | "<" | ">" | "<=" | ">=" | "==" | "!=" => {
                check_basic_calc_operator(env, scope, left, right)
            }
            "." => {
                let left_type = check_expression(env, scope, left)?;
                check_struct(env, left_type, &left.as_ref().1, right)
            }
            "->" => {
                let left_type = check_expression(env, scope, left)?;
                let Type::Pointer(ty) = left_type else {
                    return Err(TypeError::new(
                        &left.as_ref().1,
                        format!(
                            "left operand is not pointer. left is `{}`.",
                            left_type.type_name()
                        ),
                    ));
                };
                check_struct(env, *ty, &left.as_ref().1, right)
            }
            _ => {
                let left_type = check_expression(env, scope, left)?;
                let right_type = check_expression(env, scope, right)?;
                if left_type != right_type {
                    Err(TypeError::new(
                        &left.as_ref().1,
                        format!(
                            "type mismatched for operator `{}`. left type is {}, right type is {}",
                            operator,
                            left_type.type_name(),
                            right_type.type_name()
                        ),
                    ))
                } else {
                    Ok(left_type)
                }
            }
        },
        Expression::Postfix { operator, left } => match operator.as_str() {
            "++" | "--" => {
                let left_type = check_expression(env, scope, left)?;
                if left_type.type_name() != "int" {
                    Err(TypeError::new(
                        &left.1,
                        format!(
                            "type mismatched for postfix operator `{}`. left type is {}",
                            operator,
                            left_type.type_name()
                        ),
                    ))
                } else {
                    Ok(left_type)
                }
            }
            _ => Err(TypeError::new(
                &left.as_ref().1,
                format!("type mismatched for postfix operator `{}`", operator),
            )),
        },
        Expression::FunctionCall {
            function_name,
            arguments,
        } => {
            let Some(f) = env.find_function(function_name) else {
                return Err(TypeError::new(
                    exp_loc,
                    format!("function `{}` is not defined", function_name),
                ));
            };
            if f.parameters.len() != arguments.len() {
                return Err(TypeError::new(
                    exp_loc,
                    format!("wrong number of arguments, `{}`.", function_name),
                ));
            }
            let errors: Vec<String> = f
                .parameters
                .iter()
                .map(|p| {
                    env.solve_type(&p.type_ref).map_err(|e| TypeError::new(exp_loc, e)).unwrap();
                    &p.type_ref
                })
                .zip(arguments.iter())
                .enumerate()
                .flat_map(|(i, (param, arg))| match check_expression(env, scope, arg) {
                    Ok(arg_type) => {
                        let (_, loc) = arg;
                        let Ok(param_type) = env.solve_type(param).map_err(|e| TypeError::new(loc, e)) else {
                            return vec![build_error_msg(
                                loc,
                                format!("type not defined. {}.", param.type_name()),
                            )];
                        };
                        if arg_type != param_type {
                            let (_, loc) = arg;
                            vec![build_error_msg(
                                loc,
                                format!(
                                    "mismatched type for argument {} in function call, `{}`.",
                                    i + 1,
                                    function_name
                                ),
                            )]
                        } else {
                            vec![]
                        }
                    }
                    Err(e) => e.errors,
                })
                .collect();
            if errors.is_empty() {
                env.solve_type(&f.return_type_ref).map_err(|e| TypeError::new(exp_loc, e)) 
            } else {
                Err(TypeError { errors })
            }
        }
        Expression::Index { left, index } => {
            // as[x]の `x` の型がintであることをチェック
            let index_type = check_expression(env, scope, index)?;
            if index_type.type_name() != "int" {
                return Err(TypeError::new(
                    &index.as_ref().1,
                    format!(
                        "index should be int. but it is `{}`.",
                        index_type.type_name()
                    ),
                ));
            }
            // as[x]の `as` の型を解決する
            let var_type = check_expression(env, scope, left)?;
            match var_type {
                Type::Pointer(inner_type) => Ok(*inner_type),
                Type::Array { type_dec, .. } => Ok(*type_dec),
                _ => Err(TypeError::new(
                    &left.as_ref().1,
                    format!(
                        "index operand should be array. but it is `{}`.",
                        var_type.type_name()
                    ),
                )),
            }
        }
        Expression::Initializer { .. } => {
            panic!("Expression::Initializer is unexpected.")
        }
    }
}

fn check_function_declaration(
    env: &Env,
    scope: &LocalScope,
    return_type_ref: &TypeRef,
    parameters: &Vec<Parameter>,
    body: &Option<Box<StatementNode>>,
    loc: &Loc,
) -> Result<()> {
    let mut results: Vec<TypeError> = vec![];

    env.solve_type(return_type_ref)
        .map_err(|e| TypeError::new(loc, e)) 
        .err()
        .into_iter()
        .for_each(|e| {
            results.push(e);
        });
    for p in parameters {
        env
            .solve_type(&p.type_ref)
            .map_err(|e| TypeError::new(loc, e)) 
            .err()
            .into_iter()
            .for_each(|e| {
                results.push(e);
            });
    }

    if let Some(stmt) = body {
        // TODO: check `return_type_dec`
        results.extend(check_statement(env, scope, stmt));
    }
    if results.is_empty() {
        return Ok(());
    }
    Err(TypeError {
        errors: results.into_iter().flat_map(|e| e.errors).collect(),
    })
}

fn check_declarator(env: &Env, scope: &LocalScope, type_ref: &TypeRef, decl: &Declarator, loc: &Loc) -> Result<Type> {
    let Some(exp) = &decl.value else {
        return env.solve_type(type_ref).map_err(|e| TypeError::new(loc, e));
    };
    let Expression::Initializer {
        elements: init_elms,
    } = &exp.0
    else {
        // 右辺が初期化子ではない場合のチェック
        let var_type = check_expression(env, scope, exp)?;
        if type_ref.type_name() != var_type.type_name() {
            return Err(TypeError::new(
                &exp.1,
                format!(
                    "type error. initialize variable type is {}, value type is {}",
                    type_ref.type_name(),
                    var_type.type_name()
                ),
            ));
        }
        return env.solve_type(type_ref).map_err(|e| TypeError::new(&exp.1, e));
    };
    // 左辺の型情報（type_ref）を使って、右辺の初期化子をチェックする
    let loc = &exp.1;
    match type_ref {
        TypeRef::Named(_) => env.solve_type(type_ref).map_err(|e| TypeError::new(loc, e)),
        TypeRef::Pointer(_) => Err(TypeError::new(
            loc,
            "invalid initializer for pointer type".to_string(),
        )),
        TypeRef::Array { type_ref, size } => {
            if let Some(l) = size
                && init_elms.len() > (*l).try_into().unwrap()
            {
                return Err(TypeError::new(
                    loc,
                    format!(
                        "too many initializers for array of size {}, but it length is {}",
                        l,
                        init_elms.len()
                    ),
                ));
            };
            let errors: Vec<String> = init_elms
                .iter()
                .flat_map(|init_elm| {
                    match check_expression(env, scope, init_elm) {
                        Ok(init_elm_ty) => {
                            let (_, init_elm_loc) = init_elm;
                            let Ok(ty) = env.solve_type(type_ref).map_err(|e| TypeError::new(init_elm_loc, e)) else {
                                return vec![build_error_msg( init_elm_loc, format!( "type not defined. {}.", type_ref.type_name()))]
                            };
                            if ty != init_elm_ty {
                                vec![build_error_msg(
                                    init_elm_loc,
                                    format!("type mismatched for initializer. left type is {}, right type is {}", type_ref.as_ref().type_name(), init_elm_ty.type_name()),
                                )]
                            } else {
                                vec![]
                            }
                        },
                        Err(e) => e.errors,
                    }
                }).collect();
            if errors.is_empty() {
                env.solve_type(type_ref).map_err(|e| TypeError::new(loc, e))
            } else {
                Err(TypeError { errors })
            }
        }
        TypeRef::Struct(struct_ref) => {
            let tyopt: Option<Type> = match struct_ref {
                StructRef::TagName(_) => env.solve_type(type_ref).map_err(|e| TypeError::new(loc, e)).ok(),
                StructRef::Decl(struct_decl) => Some(Type::Struct(struct_decl.clone())),
            };
            let Some(Type::Struct(struct_decl)) = tyopt else {
                return Err(TypeError::new(
                    loc,
                    format!("type not defined, {}", type_ref.type_name()),
                ));
            };
            let StructDecl {
                tag_name: _,
                members: defined_members,
            } = struct_decl.clone();
            if init_elms.len() != defined_members.len() {
                return Err(TypeError::new(
                    loc,
                    format!(
                        "initializer length should be {}, but is {}",
                        defined_members.len(),
                        init_elms.len()
                    ),
                ));
            }
            let errors: Vec<String> = init_elms
                .iter()
                .zip(defined_members)
                .flat_map(|(init_elm, decl)| {
                    match check_expression(env, scope, init_elm) {
                        Ok(init_elm_ty) => {
                            let (_, init_elm_loc) = init_elm;
                            let Ok(ty) = env.solve_type(&decl.type_ref).map_err(|e| TypeError::new(init_elm_loc, e)) else {
                                return vec![build_error_msg( init_elm_loc, format!( "type not defined. {}.", type_ref.type_name()))]
                            };
                            if ty != init_elm_ty {
                                vec![build_error_msg(
                                    init_elm_loc,
                                    format!("type mismatched for initializer. left type is {}, right type is {}", decl.type_ref.type_name(), init_elm_ty.type_name())
                                )]
                            } else {
                                vec![]
                            }
                        },
                        Err(e) => e.errors,
                    }
                }).collect();
            if errors.is_empty() {
                Ok(Type::Struct(struct_decl))
            } else {
                Err(TypeError { errors })
            }
        }
    }
}

fn check_basic_calc_operator(
    env: &Env,
    scope: &LocalScope,
    left: &ExpressionNode,
    right: &ExpressionNode,
) -> Result<Type> {
    let mut errors: Vec<String> = vec![];
    let left_type = check_expression(env, scope, left)?;
    if left_type.type_name() != "int" {
        let (_, loc) = left;
        errors.push(build_error_msg(
            loc,
            format!(
                "type error. left should be `int`, but is {}",
                left_type.type_name()
            ),
        ));
    }
    let right_type = check_expression(env, scope, right)?;
    if right_type.type_name() != "int" {
        let (_, loc) = right;
        errors.push(build_error_msg(
            loc,
            format!(
                "type error. right should be `int`, but is {}",
                right_type.type_name()
            ),
        ));
    }
    if !errors.is_empty() {
        return Err(TypeError { errors });
    }
    if left_type.type_name() != right_type.type_name() {
        let (_, loc) = left;
        errors.push(build_error_msg(
            loc,
            format!(
                "type error. left type is {}, right type is {}",
                left_type.type_name(),
                right_type.type_name()
            ),
        ));
        return Err(TypeError { errors });
    }
    Ok(left_type)
}

fn check_struct(
    env: &Env,
    left_type: Type,
    left_loc: &Loc,
    right: &ExpressionNode,
) -> Result<Type> {
    // ".", "->" の左側オペランドが、structの定義に含まれているかチェック
    let Type::Struct(StructDecl {
        tag_name: _,
        members: defined_members,
    }) = left_type
    else {
        return Err(TypeError::new(
            left_loc,
            format!("left type is not struct. left type is {:?}", left_type),
        ));
    };
    // ".", "->" の右側オペランドが、structのフィールド定義に含まれているかチェック
    let (Expression::Identifier(operand_member), _) = right else {
        return Err(TypeError::new(
            &right.1,
            format!("right is not Identifier. right: {:?}", right.0),
        ));
    };
    if let Some(defined) = defined_members.iter().find(|m| m.name == *operand_member) {
        env.solve_type(&defined.type_ref).map_err(|e| TypeError::new(&right.1, e))
    } else {
        Err(TypeError::new(
            &right.1,
            format!(
                "field `{}` is not defined. defined members: {{{}}}",
                operand_member,
                defined_members
                    .into_iter()
                    .map(|m| m.name.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser, sema::scope_checker::check_scope};

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
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
            assert_eq!(errors.len(), 5);
            assert_eq!(
                "error:2:9: type error. initialize variable type is int, value type is char",
                errors[0]
            );
            assert_eq!(
                "error:3:13: type error. right should be `int`, but is char",
                errors[1]
            );
            assert_eq!(
                "error:4:12: type error. initialize variable type is char, value type is int",
                errors[2]
            );
            assert_eq!(
                "error:5:17: type error. right should be `int`, but is char",
                errors[3]
            );
            assert_eq!(
                "error:9:16: type error. right should be `int`, but is char",
                errors[4]
            );
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
        int y1 = 1 + 2;
    } else {
        int y3 = 1 + 'a';
        int y4 = 1 + 2;
        char yy = y4 + 2;
    }
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
            assert_eq!(errors.len(), 3);
            assert_eq!(
                "error:3:9: type error. condition type is char",
                errors[0]
            );
            // assert_eq!(
            //     "error:6:18: variable `y1` is not defined",
            //     errors[1]
            // );
            assert_eq!(
                "error:6:22: type error. right should be `int`, but is char",
                errors[1]
            );
            assert_eq!(
                "error:8:22: type error. initialize variable type is char, value type is int",
                errors[2]
            );
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_type_no_error_at_switch() {
        // given
        let input = r#"
int take_value() {
  return 2;
}

int main() {
  char* ans;
  switch (take_value()) {
    case 1:
      ans = "One\n";
      break;
    case 2:
      ans = "Two\n";
    case 3:
    default:
      ans = "Default\n";
  }
  return 0;
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors: _ }) = result.err() {
            assert!(false);
        } else {
            assert!(true);
        }
    }

    #[test]
    fn test_type_error_at_switch() {
        // given
        let input = r#"
int take_value() {
  return 2;
}

struct person {
    int age;
    char* name;
};

int main() {
  char* ans;
  switch (take_value()) {
    case 'a':
      ans = "One\n";
    case 2:
      ans = 2222;
    case 3:
    default:
      ans = "Default\n";
  }

  struct person p = { 30, "Dave" };
  switch (p) {
    default:
      ans = "Default\n";
  }

  return 0;
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
            assert_eq!(errors.len(), 3);
            assert_eq!(
                "error:14:10: type error. switch label type is char, but switch condition type is int",
                errors[0]
            );
            assert_eq!(
                "error:17:7: type mismatched for operator `=`. left type is char*, right type is int",
                errors[1]
            );
            assert_eq!(
                "error:24:11: type error. switch condition type is struct person",
                errors[2]
            );
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_type_no_error_at_while() {
        // given
        let input = r#"
int take_value() {
  return 2;
}

int MAX = 10;

int main() {
  char* ans;
  while (take_value() <= MAX) {
    if (take_value() == 1) {
      ans = "One\n";
    } else if (take_value() == 2) {
      ans = "Two\n";
      continue;
    } else {
      break;
    }
  }
  while (0)
    break;
  while (take_value()) {}
  return 0;
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { .. }) = result.err() {
            assert!(false);
        } else {
            assert!(true);
        }
    }

    #[test]
    fn test_type_error_at_while() {
        // given
        let input = r#"
int take_value() {
  return 2;
}

int MAX = 10;

int main() {
  char* ans;
  while ("aaaa") {
    if (take_value() == 1) {
      ans = "One\n";
    } else if (take_value() == 2) {
      ans = "Two\n";
      continue;
    } else {
      break;
    }
  }
  return 0;
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(
                "error:10:10: type error. while condition type is char*",
                errors[0]
            );
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_type_no_error_at_for() {
        // given
        let input = r#"
int take_value() {
  return 2;
}

int main() {
  char* ans;
  int i;
  for (i = 0; i < take_value(); i++) {
    if (take_value() == 1) {
      ans = "One\n";
      continue;
    } else {
      break;
    }
  }
  for (;; ++i)
    break;
  for (;;) {}
  return 0;
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { .. }) = result.err() {
            assert!(false);
        } else {
            assert!(true);
        }
    }

    #[test]
    fn test_type_error_at_for() {
        // given
        let input = r#"
int take_value() {
  return 2;
}

int main() {
  char* ans;
  int i;
  for (i = 100; "aaaaa"; ans--) {
    if (take_value() == 1) {
      ans = "One\n";
      continue;
    } else {
      break;
    }
  }
  return 0;
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
            assert_eq!(errors.len(), 2);
            assert_eq!(
                "error:9:17: type error. for condition type is char*",
                errors[0]
            );
            assert_eq!(
                "error:9:26: type mismatched for postfix operator `--`. left type is char*",
                errors[1]
            );
        } else {
            assert!(false);
        }
    }

    #[test]
    fn test_type_no_error_at_dowhile() {
        // given
        let input = r#"
int take_value() {
  return 2;
}

int MAX = 10;

int main() {
  char* ans;
  do {
    if (take_value() == 1) {
      ans = "One\n";
    } else if (take_value() == 2) {
      ans = "Two\n";
      continue;
    } else {
      break;
    }
  } while (take_value() <= MAX);
  do {
    break;
  } while (0);
  do {
  } while (take_value());
  return 0;
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
            // assert!(false);
            assert_eq!(errors.len(), 1);
            assert_eq!(
                "error:9:17: type error. for condition type is char*",
                errors[0]
            );
        } else {
            assert!(true);
        }
    }

    #[test]
    fn test_type_error_at_dowhile() {
        // given
        let input = r#"
int take_value() {
  return 2;
}

int MAX = 10;

int main() {
  char* ans;
  do {
    if (take_value() == 1) {
      ans = "One\n";
    } else if (take_value() == 2) {
      ans = "Two\n";
      continue;
    } else {
      break;
    }
  } while ("aaaa");
  return 0;
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(
                "error:19:12: type error. while condition type is char*",
                errors[0]
            );
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
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
            assert_eq!(errors.len(), 5);
            assert_eq!(
                "error:8:18: type error. initialize variable type is char, value type is int",
                errors[0]
            );
            assert_eq!(
                "error:10:5: type mismatched for operator `=`. left type is char, right type is int",
                errors[1]
            );
            assert_eq!("error:12:17: wrong number of arguments, `inc`.", errors[2]);
            assert_eq!(
                "error:13:18: mismatched type for argument 1 in function call, `inc`.",
                errors[3]
            );
            assert_eq!("error:14:17: wrong number of arguments, `inc`.", errors[4]);
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
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
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

    p.1 = 10;

    struct person p2 = { "Dave", 30 };
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
            assert_eq!(errors.len(), 11);
            assert_eq!(
                "error:9:6: type mismatched for operator `=`. left type is int, right type is char*",
                errors[0]
            );
            assert_eq!(
                "error:10:7: field `agee` is not defined. defined members: {age, name}",
                errors[1]
            );
            assert_eq!(
                "error:12:9: type mismatched for operator `=`. left type is char*, right type is int",
                errors[2]
            );
            assert_eq!(
                "error:13:11: field `namee` is not defined. defined members: {age, name}",
                errors[3]
            );
            assert_eq!(
                "error:14:5: left operand is not pointer. left is `struct person`.",
                errors[4]
            );
            assert_eq!(
                "error:17:12: type mismatched for operator `=`. left type is char*, right type is int",
                errors[5]
            );
            assert_eq!(
                "error:18:13: field `namee` is not defined. defined members: {age, name}",
                errors[6]
            );
            assert_eq!(
                "error:19:6: left type is not struct. left type is Pointer(Struct(StructDecl { tag_name: Some(\"person\"), members: [StructMember { type_ref: Named(\"int\"), name: \"age\" }, StructMember { type_ref: Pointer(Named(\"char\")), name: \"name\" }] }))",
                errors[7]
            );
            assert_eq!(
                "error:21:7: right is not Identifier. right: Int(1)",
                errors[8]
            );
            assert_eq!(
                "error:23:26: type mismatched for initializer. left type is int, right type is char*",
                errors[9]
            );
            assert_eq!(
                "error:23:34: type mismatched for initializer. left type is char*, right type is int",
                errors[10]
            );
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

    int zs[3] = { 1, 2, 3 };
    int zs2[] = { 1, 2, 3 };
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
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

    int zs[3] = { 1, "bar", 3 };
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let env = check_scope(&ast).unwrap();
        let result = check_type(&ast, &env);

        // then
        if let Some(TypeError { errors }) = result.err() {
            assert_eq!(errors.len(), 10);
            assert_eq!(
                "error:4:7: type mismatched for operator `=`. left type is int, right type is char*",
                errors[0]
            );
            assert_eq!(
                "error:5:8: index should be int. but it is `char*`.",
                errors[1]
            );
            assert_eq!(
                "error:8:5: index operand should be array. but it is `int`.",
                errors[2]
            );
            assert_eq!(
                "error:11:8: index should be int. but it is `int*`.",
                errors[3]
            );
            assert_eq!(
                "error:14:8: index should be int. but it is `int*`.",
                errors[4]
            );
            assert_eq!(
                "error:17:7: type mismatched for operator `=`. left type is int, right type is char*",
                errors[5]
            );
            assert_eq!(
                "error:18:8: index should be int. but it is `char*`.",
                errors[6]
            );
            assert_eq!(
                "error:19:8: index should be int. but it is `int*`.",
                errors[7]
            );
            assert_eq!(
                "error:20:8: index should be int. but it is `int*`.",
                errors[8]
            );
            assert_eq!(
                "error:22:22: type mismatched for initializer. left type is int, right type is char*",
                errors[9]
            );
        } else {
            assert!(false);
        }
    }
}
