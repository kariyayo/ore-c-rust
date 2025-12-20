use std::{
    collections::{HashMap, HashSet},
    fmt,
    hash::Hash,
    vec,
};
use uuid::Uuid;

use crate::parser::ast::{
    Declarator, Expression, ExpressionNode, ExternalItem, FunctionDecl, Loc, Parameter, Program,
    Statement, StatementNode, StructDecl, StructRef, SwitchBlock, SwitchLabel, TypeRef,
};

#[derive(Debug)]
pub struct Error {
    errors: Vec<String>,
}

impl Error {
    fn new(loc: &Loc, msg: String) -> Error {
        Error {
            errors: vec![build_error_msg(loc, msg)],
        }
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

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
enum Type {
    Basic(String),
    Struct(StructDecl),
    Pointer(Box<Type>),
    Array {
        type_dec: Box<Type>,
        size: Option<u32>,
    },
}

impl Type {
    fn type_name(&self) -> String {
        match self {
            Type::Basic(name) => name.to_string(),
            Type::Struct(struct_decl) => {
                let tag_name = struct_decl.tag_name.to_owned().unwrap_or_default();
                let members = struct_decl
                    .members
                    .iter()
                    .map(|member| member.name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                if members.is_empty() || !tag_name.is_empty() {
                    format!("struct {}", tag_name)
                } else {
                    format!("struct {{{}}}", members)
                }
            }
            Type::Pointer(ty) => format!("{}*", ty.type_name()),
            Type::Array { type_dec, .. } => format!("{}[]", type_dec.type_name()),
        }
    }
}

#[derive(Debug)]
pub struct TypeTable {
    entities: HashMap<String, Type>,
    types: HashSet<Type>,
}

impl TypeTable {
    fn new() -> TypeTable {
        let entities = generate_c_types();
        let mut types: HashSet<Type> = HashSet::new();
        for value in entities.values() {
            types.insert(value.clone());
        }
        TypeTable { entities, types }
    }
    fn put_struct(&mut self, ty: StructDecl) {
        self.entities.insert(
            ty.clone().tag_name.unwrap_or(Uuid::new_v4().to_string()),
            Type::Struct(ty.clone()),
        );
        self.types.insert(Type::Struct(ty.clone()));
    }
    fn find_basic(&self, ty: String) -> Option<Type> {
        self.types.get(&Type::Basic(ty)).cloned()
    }
    fn find_by_struct_ref(&self, struct_ref: StructRef) -> Option<Type> {
        match struct_ref {
            StructRef::TagName(name) => self.entities.get(&name.to_string()).cloned(),
            StructRef::Decl(struct_decl) => Some(Type::Struct(struct_decl)),
        }
    }
}

#[derive(Debug)]
pub struct Functions {
    entities: HashMap<String, FunctionDecl>,
}

impl Functions {
    fn new() -> Functions {
        Functions {
            entities: HashMap::new(),
        }
    }
    fn put(&mut self, function: FunctionDecl) {
        self.entities.insert(function.name.to_string(), function);
    }
    fn find(&self, name: &str) -> Option<FunctionDecl> {
        self.entities.get(name).cloned()
    }
}

#[derive(Debug)]
struct LocalScope<'a> {
    parent: Option<&'a LocalScope<'a>>,
    entities: HashMap<String, TypeRef>,
}

impl<'a> LocalScope<'a> {
    fn new(parent: Option<&'a LocalScope<'_>>) -> LocalScope<'a> {
        LocalScope {
            parent,
            entities: HashMap::new(),
        }
    }
    fn put(&mut self, name: &str, type_ref: TypeRef) {
        self.entities.insert(name.to_string(), type_ref);
    }

    fn find(&self, name: &str) -> Option<TypeRef> {
        self.entities
            .get(name)
            .cloned()
            .or_else(|| self.parent.and_then(|p| p.find(name)))
    }
}

#[derive(Debug)]
struct Env<'a> {
    type_table: &'a mut TypeTable,
    functions: &'a mut Functions,
    scope: LocalScope<'a>,
}

impl<'a> Env<'a> {
    fn put_struct_type(&mut self, ty: StructDecl) {
        self.type_table.put_struct(ty);
    }

    fn solve_type(&self, type_ref: &TypeRef, loc: &Loc) -> Result<Type> {
        match type_ref {
            TypeRef::Named(name) => self
                .type_table
                .find_basic(name.to_string())
                .ok_or(Error::new(
                    loc,
                    format!("variable type `{}` is not defined", type_ref.type_name()),
                )),
            TypeRef::Pointer(type_ref) => self
                .solve_type(type_ref, loc)
                .map(|ty| Type::Pointer(Box::new(ty))),
            TypeRef::Array { type_ref, size } => {
                self.solve_type(type_ref, loc).map(|ty| Type::Array {
                    type_dec: Box::new(ty),
                    size: *size,
                })
            }
            TypeRef::Struct(struct_ref) => self
                .type_table
                .find_by_struct_ref(struct_ref.clone())
                .ok_or(Error::new(
                    loc,
                    format!("struct type `{}` is not defined", type_ref.type_name()),
                )),
        }
    }

    fn put_function(&mut self, function: FunctionDecl) {
        self.functions.put(function);
    }

    fn find_function(&self, name: &str) -> Option<FunctionDecl> {
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
    let mut type_table = TypeTable::new();
    let mut functions = Functions::new();
    let global_scope = LocalScope::new(None);

    let mut results: Vec<Error> = vec![];
    let mut env = Env {
        type_table: &mut type_table,
        functions: &mut functions,
        scope: global_scope,
    };

    for item_node in &ast.external_item_nodes {
        let (item, external_item_loc) = item_node;
        match item {
            ExternalItem::StructDeclNode(struct_decl) => {
                let StructDecl {
                    tag_name: _,
                    members,
                } = struct_decl;
                let es: Vec<Error> = members
                    .iter()
                    .filter_map(|member| env.solve_type(&member.type_ref, external_item_loc).err())
                    .collect();
                if !es.is_empty() {
                    results.extend(es);
                } else {
                    env.put_struct_type(struct_decl.clone());
                }
            }
            ExternalItem::VarDeclNode(items) => {
                for (type_ref, decl) in items {
                    if let Err(e) = check_declarator(&env, type_ref, decl, external_item_loc) {
                        results.push(e);
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
                    return_type_ref: return_type_dec,
                    name: _,
                    parameters,
                    body,
                } = function;
                if let Err(e) = check_function_declaration(
                    &mut env,
                    return_type_dec,
                    parameters,
                    body,
                    external_item_loc,
                ) {
                    results.push(e);
                } else {
                    env.put_function(function.clone());
                }
            }
        }
    }
    if results.is_empty() {
        return Ok(());
    }
    Err(Error {
        errors: results.into_iter().flat_map(|e| e.errors).collect(),
    })
}

fn check_statement(env: &mut Env, stmt_node: &StatementNode) -> Vec<Error> {
    let mut results: Vec<Error> = vec![];
    let (stmt, stmt_loc) = stmt_node;
    match stmt {
        Statement::Return(expression) => {
            if let Err(e) = check_return_statement(env, expression) {
                results.push(e);
            }
        }
        Statement::Break | Statement::Continue => {
            // NOP
        }
        Statement::VarDecl(items) => {
            results.append(&mut check_var_decl_statement(env, items, stmt_loc));
        }
        Statement::Block(statements) => {
            results.append(&mut check_block_statement(env, statements));
        }
        Statement::If {
            condition,
            consequence,
            alternative,
        } => {
            results.append(&mut check_if_statement(
                env,
                condition,
                consequence,
                alternative,
            ));
        }
        Statement::Switch {
            condition,
            switch_block,
        } => {
            results.append(&mut check_switch_statement(env, condition, switch_block));
        }
        Statement::While { condition, body } => {
            results.append(&mut check_while_statement(env, condition, body));
        }
        Statement::DoWhile { body, condition } => {
            results.append(&mut check_while_statement(env, condition, body));
        }
        Statement::For {
            init,
            condition,
            post,
            body,
        } => {
            results.append(&mut check_for_statement(env, init, condition, post, body));
        }
        Statement::ExpressionStatement(expression) => {
            if let Err(e) = check_expression(env, expression) {
                results.push(e);
            }
        }
    }
    results
}

fn check_return_statement(env: &mut Env, expression: &Option<(Expression, Loc)>) -> Result<Type> {
    if let Some(exp) = expression {
        check_expression(env, exp)
    } else {
        Ok(Type::Basic("void".to_string()))
    }
}

fn check_var_decl_statement(
    env: &mut Env,
    items: &Vec<(TypeRef, Declarator)>,
    loc: &Loc,
) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];
    for (type_ref, decl) in items {
        if let Err(e) = check_declarator(env, type_ref, decl, loc) {
            errors.push(e);
        } else {
            env.put_vardecl(&decl.name, type_ref.clone());
        }
    }
    errors
}

fn check_block_statement(env: &mut Env, statements: &[StatementNode]) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];
    let local_scope = LocalScope::new(Some(&env.scope));
    let mut new_env = Env {
        type_table: env.type_table,
        functions: env.functions,
        scope: local_scope,
    };
    for stmt in statements {
        errors.append(&mut check_statement(&mut new_env, stmt));
    }
    errors
}

fn check_if_statement(
    env: &mut Env,
    condition: &(Expression, Loc),
    consequence: &(Statement, Loc),
    alternative: &Option<Box<(Statement, Loc)>>,
) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];
    match check_expression(env, condition) {
        Ok(condition_type) => {
            if condition_type.type_name() != "int" {
                errors.push(Error::new(
                    &condition.1,
                    format!(
                        "type error. condition type is {}",
                        condition_type.type_name()
                    ),
                ));
            }
            errors.extend(check_statement(env, consequence));
            if let Some(alt_stmt) = alternative {
                errors.extend(check_statement(env, alt_stmt));
            }
        }
        Err(e) => errors.push(e),
    };
    errors
}

fn check_switch_statement(
    env: &mut Env,
    condition: &(Expression, Loc),
    switch_block: &SwitchBlock,
) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];
    match check_expression(env, condition) {
        Ok(condition_type) => {
            match condition_type.type_name().as_str() {
                "int" | "char" | "short" | "long" => {
                    // NOP
                }
                _ => {
                    errors.push(Error::new(
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
                    match check_expression(env, label_exp) {
                        Ok(label_ty) => {
                            if label_ty != condition_type {
                                errors.push(Error::new(
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
                errors.extend(check_statement(env, stmt));
            }
        }
        Err(e) => errors.push(e),
    }
    errors
}

fn check_while_statement(
    env: &mut Env,
    condition: &(Expression, Loc),
    body: &(Statement, Loc),
) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];
    match check_expression(env, condition) {
        Ok(condition_type) => {
            if condition_type.type_name().as_str() != "int" {
                errors.push(Error::new(
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
    errors.extend(check_statement(env, body));
    errors
}

fn check_for_statement(
    env: &mut Env,
    init: &Option<(Expression, Loc)>,
    condition: &Option<(Expression, Loc)>,
    post: &Option<(Expression, Loc)>,
    body: &(Statement, Loc),
) -> Vec<Error> {
    let mut errors: Vec<Error> = vec![];
    if let Some(Err(e)) = init.as_ref().map(|exp| check_expression(env, exp)) {
        errors.push(e);
    }
    if let Some(exp) = condition.as_ref() {
        match check_expression(env, exp) {
            Ok(condition_type) => {
                if condition_type.type_name().as_str() != "int" {
                    errors.push(Error::new(
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
    if let Some(Err(e)) = post.as_ref().map(|exp| check_expression(env, exp)) {
        errors.push(e);
    }
    errors.extend(check_statement(env, body));
    errors
}

fn check_expression(env: &Env, exp_node: &ExpressionNode) -> Result<Type> {
    let (exp, exp_loc) = exp_node;
    match exp {
        Expression::Int(_) => Ok(Type::Basic("int".to_string())),
        Expression::CharacterLiteral(_) => Ok(Type::Basic("char".to_string())),
        Expression::StringLiteral(_) => {
            Ok(Type::Pointer(Box::new(Type::Basic("char".to_string()))))
        }
        Expression::Identifier(name) => match env.find_vardecl(name) {
            Some(type_ref) => env.solve_type(&type_ref, exp_loc),
            None => Err(Error::new(
                exp_loc,
                format!("variable `{}` is not defined", name),
            )),
        },
        Expression::Prefix { operator, right } => match operator.as_str() {
            "!" | "-" | "++" | "--" => {
                let right_type = check_expression(env, right)?;
                if right_type.type_name() != "int" {
                    Err(Error::new(
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
                let right_type = check_expression(env, right)?;
                let Type::Pointer(ty) = right_type else {
                    return Err(Error::new(
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
                let right_type = check_expression(env, right)?;
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
        } => {
            match operator.as_str() {
                "+" | "-" | "*" | "/" | "%" | "<" | ">" | "<=" | ">=" | "==" | "!=" => {
                    check_basic_calc_operator(env, left, right)
                }
                "." => {
                    let left_type = check_expression(env, left)?;
                    check_struct(env, left_type, &left.as_ref().1, right)
                }
                "->" => {
                    let left_type = check_expression(env, left)?;
                    let Type::Pointer(ty) = left_type else {
                        return Err(Error::new(
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
        }
        Expression::Postfix { operator, left } => match operator.as_str() {
            "++" | "--" => {
                let left_type = check_expression(env, left)?;
                if left_type.type_name() != "int" {
                    Err(Error::new(
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
            _ => Err(Error::new(
                &left.as_ref().1,
                format!("type mismatched for postfix operator `{}`", operator),
            )),
        },
        Expression::FunctionCall {
            function_name,
            arguments,
        } => {
            let Some(f) = env.find_function(function_name) else {
                return Err(Error::new(
                    exp_loc,
                    format!("function `{}` is not defined", function_name),
                ));
            };
            if f.parameters.len() != arguments.len() {
                return Err(Error::new(
                    exp_loc,
                    format!("wrong number of arguments, `{}`.", function_name),
                ));
            }
            let errors: Vec<String> = f
                .parameters
                .iter()
                .map(|p| {
                    env.solve_type(&p.type_ref, exp_loc).unwrap();
                    &p.type_ref
                })
                .zip(arguments.iter())
                .enumerate()
                .flat_map(|(i, (param, arg))| match check_expression(env, arg) {
                    Ok(arg_type) => {
                        let (_, loc) = arg;
                        let Ok(param_type) = env.solve_type(param, loc) else {
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
                env.solve_type(&f.return_type_ref, exp_loc)
            } else {
                Err(Error { errors })
            }
        }
        Expression::Index { left, index } => {
            // as[x]の `x` の型がintであることをチェック
            let index_type = check_expression(env, index)?;
            if index_type.type_name() != "int" {
                return Err(Error::new(
                    &index.as_ref().1,
                    format!(
                        "index should be int. but it is `{}`.",
                        index_type.type_name()
                    ),
                ));
            }
            // as[x]の `as` の型を解決する
            let var_type = check_expression(env, left)?;
            match var_type {
                Type::Pointer(inner_type) => Ok(*inner_type),
                Type::Array { type_dec, .. } => Ok(*type_dec),
                _ => Err(Error::new(
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
    env: &mut Env,
    return_type_ref: &TypeRef,
    parameters: &Vec<Parameter>,
    body: &Option<Box<StatementNode>>,
    loc: &Loc,
) -> Result<()> {
    let mut results: Vec<Error> = vec![];

    env.solve_type(return_type_ref, loc)
        .err()
        .into_iter()
        .for_each(|e| {
            results.push(e);
        });

    let local_scope = LocalScope::new(Some(&env.scope));
    let mut new_env = Env {
        type_table: env.type_table,
        functions: env.functions,
        scope: local_scope,
    };
    for p in parameters {
        new_env
            .solve_type(&p.type_ref, loc)
            .err()
            .into_iter()
            .for_each(|e| {
                results.push(e);
            });
        new_env.put_vardecl(&p.name, p.type_ref.clone());
    }

    if let Some(stmt) = body {
        // TODO: check `return_type_dec`
        results.extend(check_statement(&mut new_env, stmt));
    }
    if results.is_empty() {
        return Ok(());
    }
    Err(Error {
        errors: results.into_iter().flat_map(|e| e.errors).collect(),
    })
}

fn check_declarator(env: &Env, type_ref: &TypeRef, decl: &Declarator, loc: &Loc) -> Result<Type> {
    let Some(exp) = &decl.value else {
        return env.solve_type(type_ref, loc);
    };
    let Expression::Initializer {
        elements: init_elms,
    } = &exp.0
    else {
        // 右辺が初期化子ではない場合のチェック
        let var_type = check_expression(env, exp)?;
        if type_ref.type_name() != var_type.type_name() {
            return Err(Error::new(
                &exp.1,
                format!(
                    "type error. initialize variable type is {}, value type is {}",
                    type_ref.type_name(),
                    var_type.type_name()
                ),
            ));
        }
        return env.solve_type(type_ref, &exp.1);
    };
    // 左辺の型情報（type_ref）を使って、右辺の初期化子をチェックする
    let loc = &exp.1;
    match type_ref {
        TypeRef::Named(_) => env.solve_type(type_ref, loc),
        TypeRef::Pointer(_) => Err(Error::new(
            loc,
            "invalid initializer for pointer type".to_string(),
        )),
        TypeRef::Array { type_ref, size } => {
            if let Some(l) = size {
                if init_elms.len() > (*l).try_into().unwrap() {
                    return Err(Error::new(
                        loc,
                        format!(
                            "too many initializers for array of size {}, but it length is {}",
                            l,
                            init_elms.len()
                        ),
                    ));
                }
            };
            let errors: Vec<String> = init_elms
                .iter()
                .flat_map(|init_elm| {
                    match check_expression(env, init_elm) {
                        Ok(init_elm_ty) => {
                            let (_, init_elm_loc) = init_elm;
                            let Ok(ty) = env.solve_type(type_ref, init_elm_loc) else {
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
                env.solve_type(type_ref, loc)
            } else {
                Err(Error { errors })
            }
        }
        TypeRef::Struct(struct_ref) => {
            let tyopt: Option<Type> = match struct_ref {
                StructRef::TagName(_) => env.solve_type(type_ref, loc).ok(),
                StructRef::Decl(struct_decl) => Some(Type::Struct(struct_decl.clone())),
            };
            let Some(Type::Struct(struct_decl)) = tyopt else {
                return Err(Error::new(
                    loc,
                    format!("type not defined, {}", type_ref.type_name()),
                ));
            };
            let StructDecl {
                tag_name: _,
                members: defined_members,
            } = struct_decl.clone();
            if init_elms.len() != defined_members.len() {
                return Err(Error::new(
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
                    match check_expression(env, init_elm) {
                        Ok(init_elm_ty) => {
                            let (_, init_elm_loc) = init_elm;
                            let Ok(ty) = env.solve_type(&decl.type_ref, init_elm_loc) else {
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
                Err(Error { errors })
            }
        }
    }
}

fn check_basic_calc_operator(
    env: &Env,
    left: &ExpressionNode,
    right: &ExpressionNode,
) -> Result<Type> {
    let mut errors: Vec<String> = vec![];
    let left_type = check_expression(env, left)?;
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
    let right_type = check_expression(env, right)?;
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
        return Err(Error { errors });
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
        return Err(Error { errors });
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
        return Err(Error::new(
            left_loc,
            format!("left type is not struct. left type is {:?}", left_type),
        ));
    };
    // ".", "->" の右側オペランドが、structのフィールド定義に含まれているかチェック
    let (Expression::Identifier(operand_member), _) = right else {
        return Err(Error::new(
            &right.1,
            format!("right is not Identifier. right: {:?}", right.0),
        ));
    };
    if let Some(defined) = defined_members.iter().find(|m| m.name == *operand_member) {
        env.solve_type(&defined.type_ref, &right.1)
    } else {
        Err(Error::new(
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

fn generate_c_types() -> HashMap<String, Type> {
    let mut types = HashMap::new();

    types.insert("int".to_string(), Type::Basic("int".to_string()));
    types.insert("char".to_string(), Type::Basic("char".to_string()));

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
        if let Some(Error { errors }) = result.err() {
            assert_eq!(errors.len(), 5);
            assert_eq!(
                true,
                errors[0].starts_with("error:2:9: type error. initialize variable type is"),
                "actual message: `{}`",
                errors[0]
            );
            assert_eq!(
                true,
                errors[1].starts_with("error:3:13: type error. right should be `int`, but is"),
                "actual message: `{}`",
                errors[1]
            );
            assert_eq!(
                true,
                errors[2].starts_with("error:4:12: type error. initialize variable type is"),
                "actual message: `{}`",
                errors[2]
            );
            assert_eq!(
                true,
                errors[3].starts_with("error:5:17: type error. right should be `int`, but is"),
                "actual message: `{}`",
                errors[3]
            );
            assert_eq!(
                true,
                errors[4].starts_with("error:9:16: variable `c` is not defined"),
                "actual message: `{}`",
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
        int y2 = y1;
        int y3 = 1 + 'a';
        int y4 = 1 + 2;
        char yy = y4 + 2;
    }
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error { errors }) = result.err() {
            assert_eq!(errors.len(), 4);
            assert_eq!(
                true,
                errors[0].starts_with("error:3:9: type error. condition type is"),
                "actual message: `{}`",
                errors[0]
            );
            assert_eq!(
                true,
                errors[1].starts_with("error:6:18: variable `y1` is not defined"),
                "actual message: `{}`",
                errors[1]
            );
            assert_eq!(
                true,
                errors[2].starts_with("error:7:22: type error. right should be `int`, but is char"),
                "actual message: `{}`",
                errors[2]
            );
            assert_eq!(
                true,
                errors[3].starts_with(
                    "error:9:22: type error. initialize variable type is char, value type is int"
                ),
                "actual message: `{}`",
                errors[3]
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
        let result = check_type(&ast);

        // then
        if let Some(Error { errors: _ }) = result.err() {
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
        let result = check_type(&ast);

        // then
        if let Some(Error { errors }) = result.err() {
            assert_eq!(errors.len(), 3);
            assert_eq!(
                true,
                errors[0].starts_with("error:14:10: type error. switch label type is char, but switch condition type is int"),
                "actual message: `{}`",
                errors[0]
            );
            assert_eq!(
                true,
                errors[1].starts_with("error:17:7: type mismatched for operator `=`. left type is char*, right type is int"),
                "actual message: `{}`",
                errors[1]
            );
            assert_eq!(
                true,
                errors[2]
                    .starts_with("error:24:11: type error. switch condition type is struct person"),
                "actual message: `{}`",
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
        let result = check_type(&ast);

        // then
        if let Some(Error { .. }) = result.err() {
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
        let result = check_type(&ast);

        // then
        if let Some(Error { errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(
                true,
                errors[0].starts_with("error:10:10: type error. while condition type is char*"),
                "actual message: `{}`",
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
        let result = check_type(&ast);

        // then
        if let Some(Error { .. }) = result.err() {
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
        let result = check_type(&ast);

        // then
        if let Some(Error { errors }) = result.err() {
            assert_eq!(errors.len(), 2);
            assert_eq!(
                true,
                errors[0].starts_with("error:9:17: type error. for condition type is char*"),
                "actual message: `{}`",
                errors[0]
            );
            assert_eq!(
                true,
                errors[1].starts_with(
                    "error:9:26: type mismatched for postfix operator `--`. left type is char*"
                ),
                "actual message: `{}`",
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
        let result = check_type(&ast);

        // then
        if let Some(Error { .. }) = result.err() {
            assert!(false);
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
        let result = check_type(&ast);

        // then
        if let Some(Error { errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(
                true,
                errors[0].starts_with("error:19:12: type error. while condition type is char*"),
                "actual message: `{}`",
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
        if let Some(Error { errors }) = result.err() {
            assert_eq!(errors.len(), 6);
            assert_eq!("error:7:20: function `random` is not defined", errors[0]);
            assert_eq!(
                "error:9:18: type error. initialize variable type is char, value type is int",
                errors[1]
            );
            assert_eq!("error:11:5: type mismatched for operator `=`. left type is char, right type is int", errors[2]);
            assert_eq!("error:13:17: wrong number of arguments, `inc`.", errors[3]);
            assert_eq!(
                "error:14:18: mismatched type for argument 1 in function call, `inc`.",
                errors[4]
            );
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
        if let Some(Error { errors }) = result.err() {
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
        if let Some(Error { errors }) = result.err() {
            assert_eq!(errors.len(), 12);
            assert_eq!("error:9:6: type mismatched for operator `=`. left type is int, right type is char*", errors[0]);
            assert_eq!(
                "error:10:7: field `agee` is not defined. defined members: {age, name}",
                errors[1]
            );
            assert_eq!("error:12:9: type mismatched for operator `=`. left type is char*, right type is int", errors[2]);
            assert_eq!(
                "error:13:11: field `namee` is not defined. defined members: {age, name}",
                errors[3]
            );
            assert_eq!(
                "error:14:5: left operand is not pointer. left is `struct person`.",
                errors[4]
            );
            assert_eq!("error:17:12: type mismatched for operator `=`. left type is char*, right type is int", errors[5]);
            assert_eq!(
                "error:18:13: field `namee` is not defined. defined members: {age, name}",
                errors[6]
            );
            assert_eq!("error:19:6: left type is not struct. left type is Pointer(Struct(StructDecl { tag_name: Some(\"person\"), members: [StructMember { type_ref: Named(\"int\"), name: \"age\" }, StructMember { type_ref: Pointer(Named(\"char\")), name: \"name\" }] }))", errors[7]);
            assert_eq!("error:21:5: variable `pp` is not defined", errors[8]);
            assert_eq!(
                "error:22:7: right is not Identifier. right: Int(1)",
                errors[9]
            );
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

    int zs[3] = { 1, 2, 3 };
    int zs2[] = { 1, 2, 3 };
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error { errors }) = result.err() {
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

    int zs[4] = { 1, "bar", 3, aaa };
}
"#;
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error { errors }) = result.err() {
            assert_eq!(errors.len(), 11);
            assert_eq!("error:4:7: type mismatched for operator `=`. left type is int, right type is char*", errors[0]);
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
            assert_eq!("error:17:7: type mismatched for operator `=`. left type is int, right type is char*", errors[5]);
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
            assert_eq!("error:22:22: type mismatched for initializer. left type is int, right type is char*", errors[9]);
            assert_eq!("error:22:32: variable `aaa` is not defined", errors[10]);
        } else {
            assert!(false);
        }
    }
}
