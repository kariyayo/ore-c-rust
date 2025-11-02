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
    fn contains(&self, type_ref: &TypeRef) -> bool {
        self.entities.contains(type_ref)
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
                // TODO: check struct's fields
                env.put_type(type_ref.clone());
            },
            ExternalItem::VarDecl(items) => {
                for (type_ref, decl) in items {
                    if let Err(e) = check_declarator(&mut env, type_ref, decl) {
                        results.push(e);
                    } else {
                        env.put_vardecl(decl.name.as_str(), type_ref.clone());
                    }
                }
            },
            ExternalItem::FunctionDecl(function) => {
                let Function { return_type_dec, name, parameters, body } = function;
                if let Err(e) = check_function_declaration(&mut env, return_type_dec, name, parameters, body) {
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
    let (stmt, _) = stmt_node;
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
                if let Err(e) = check_declarator(env, type_ref, decl) {
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
        Expression::StringLiteral(_) => todo!(),
        Expression::Identifier(name) => {
            match env.find_vardecl(name) {
                Some(type_ref) => {
                    Ok(type_ref)
                },
                None => {
                    Err(Error::new(exp_loc, format!("variable `{}` is not defined", name)))
                }
            }
        },
        Expression::PrefixExpression { operator, right } => todo!(),
        Expression::InfixExpression { operator, left, right } => {
            match operator.as_str() {
                "+" | "-" | "*" | "/" | "%" | "<" | ">" | "<=" | ">=" | "==" | "!=" => {
                    check_basic_calc_operator(env, left, right)
                }
                "[" => todo!(),
                "." => todo!(),
                "->" => todo!(),
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
            match env.find_function(function_name) {
                None => {
                    Err(Error::new(exp_loc, format!("function `{}` is not defined", function_name)))
                },
                Some(f) => {
                    if f.parameters.len() != arguments.len() {
                        return Err(Error::new(exp_loc, format!("wrong number of arguments, `{}`.", function_name)))
                    }
                    let errors: Vec<String> = f.parameters.iter()
                        .zip(arguments.iter())
                        .enumerate()
                        .flat_map(|(i, (param, arg))| {
                            match check_expression(env, arg) {
                                Ok(arg_type) => {
                                    if arg_type != param.type_dec {
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
            }
        },
        Expression::InitializerExpression { elements } => todo!(),
        Expression::IndexExpression { left, index } => todo!(),
    }
} 

fn check_function_declaration(
    env: &mut Env,
    return_type_dec: &TypeRef,
    name: &str,
    parameters: &Vec<Parameter>,
    body: &Option<Box<StatementNode>>,
) -> Result<()> {
    let mut results: Vec<Error> = vec![];

    if !env.is_defined_type(return_type_dec) {
        results.push(Error { errors: vec![format!("return type is not defined: {:?}", return_type_dec.type_name())] });
    }

    let local_scope = LocalScope { parent: Some(&env.scope), entities: HashMap::new() };
    let mut new_env = Env { type_table: env.type_table, functions: env.functions, scope: local_scope };
    for p in parameters {
        if !new_env.is_defined_type(&p.type_dec) {
            results.push(Error { errors: vec![format!("parameter type is not defined: {:?}", p.type_dec.type_name())] });
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

fn check_declarator(env: &Env, type_ref: &TypeRef, decl: &Declarator) -> Result<TypeRef> {
    if let Some(exp) = &decl.value {
        let var_type = check_expression(env, exp)?;
        if type_ref.type_name() != var_type.type_name() {
            return Err(Error::new(
                &exp.1,
                format!("type error. initialize variable type is {}, value type is {}", type_ref.type_name(), var_type.type_name())
            ));
        }
    }
    Ok(type_ref.clone())
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
}
