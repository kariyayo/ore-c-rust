use std::fmt;

use crate::parser::ast::{Declarator, Expression, ExternalItem, Program, Statement, TypeRef};

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

pub fn check_type(ast: &Program) -> Result<()> {
    let mut results: Vec<Error> = vec![];
    for item in &ast.external_items {
        match item {
            ExternalItem::Struct(type_ref) => {
                todo!();
            },
            ExternalItem::VarDecl(items) => {
                for (type_ref, decl) in items {
                    if let Err(e) = check_declarator(type_ref, decl) {
                        results.push(e);
                    }
                }
            },
            ExternalItem::FunctionDecl { return_type_dec, name, parameters, body } => {
                // TODO:
                if let Some(stmt) = body {
                    results.extend(check_statement(stmt));
                }
            },
        }
    }
    if results.is_empty() {
        return Ok(());
    }
    Err(Error { errors: results.into_iter().flat_map(|e| e.errors).collect() })
}

fn check_statement(stmt: &Statement) -> Vec<Error> {
    let mut results: Vec<Error> = vec![];
    match stmt {
        Statement::Return(expression) => todo!(),
        Statement::Break => todo!(),
        Statement::Continue => todo!(),
        Statement::VarDecl(items) => {
            for (type_ref, decl) in items {
                if let Err(e) = check_declarator(type_ref, decl) {
                    results.push(e);
                }
            }
        }
        Statement::Block(statements) => {
            statements.iter().for_each(|stmt| {
                let mut errors = check_statement(stmt);
                results.append(&mut errors);
            })
        },
        Statement::If { condition, consequence, alternative } => {
            match check_expression(condition) {
                Ok(condition_type) => {
                    if condition_type.type_name() != "int" {
                        results.push(Error { errors: vec![format!("type error. condition type is {}", condition_type.type_name())] });
                    }
                    results.extend(check_statement(&consequence));
                    if let Some(alt_stmt) = alternative {
                        results.extend(check_statement(alt_stmt));
                    }
                },
                Err(e) => results.push(e),
            }
        },
        Statement::Switch { condition, switch_block } => todo!(),
        Statement::While { condition, body } => todo!(),
        Statement::DoWhile { body, condition } => todo!(),
        Statement::For { init, condition, post, body } => todo!(),
        Statement::ExpressionStatement(expression) => todo!(),
    }
    results
}

fn check_expression(exp: &Expression) -> Result<TypeRef> {
    match exp {
        Expression::Int(_) => {
            Ok(TypeRef::Named("int".to_string()))
        }
        Expression::CharacterLiteral(_) => {
            Ok(TypeRef::Named("char".to_string()))
        },
        Expression::StringLiteral(_) => todo!(),
        Expression::Identifier(_) => todo!(),
        Expression::PrefixExpression { operator, right } => todo!(),
        Expression::InfixExpression { operator, left, right } => {
            match operator.as_str() {
                "+" | "-" | "*" | "/" | "%" | "<" | ">" | "<=" | ">=" | "==" | "!=" => {
                    check_basic_calc_operator(left, right)
                }
                _ => {
                    check_expression(left)?;
                    check_expression(right)
                }
            }
        },
        Expression::PostfixExpression { operator, left } => todo!(),
        Expression::FunctionCallExpression { function_name, arguments } => todo!(),
        Expression::InitializerExpression { elements } => todo!(),
        Expression::IndexExpression { left, index } => todo!(),
    }
} 

fn check_declarator(type_ref: &TypeRef, decl: &Declarator) -> Result<TypeRef> {
    if let Some(exp) = &decl.value {
        let var_type = check_expression(exp)?;
        if type_ref.type_name() != var_type.type_name() {
            return Err(Error {
                errors: vec![format!("type error. initialize variable type is {}, value type is {}", type_ref.type_name(), var_type.type_name())]
            });
        }
    }
    Ok(type_ref.clone())
}

fn check_basic_calc_operator(left: &Expression, right: &Expression) -> Result<TypeRef> {
    let mut errors: Vec<String> = vec![];
    let left_type = check_expression(left)?;
    if left_type.type_name() != "int" {
        errors.push(format!("type error. left is {}", left_type.type_name()));
    }
    let right_type = check_expression(right)?;
    if right_type.type_name() != "int" {
        errors.push(format!("type error. right is {}", right_type.type_name()));
    }
    if !errors.is_empty() {
        return Err(Error { errors });
    }
    if left_type.type_name() != right_type.type_name() {
        errors.push(format!( "type error. left type is {}, right type is {}", left_type.type_name(), right_type.type_name(),));
        return Err(Error { errors });
    }
    Ok(left_type)
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
int cond = 1 < 'a';
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_type(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert_eq!(errors.len(), 4);
            assert_eq!(true, errors[0].starts_with("type error. initialize variable type is"), "actual message: `{}`", errors[0]);
            assert_eq!(true, errors[1].starts_with("type error. right is"), "actual message: `{}`", errors[1]);
            assert_eq!(true, errors[2].starts_with("type error. initialize variable type is"), "actual message: `{}`", errors[2]);
            assert_eq!(true, errors[3].starts_with("type error. right is"), "actual message: `{}`", errors[3]);
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
            assert_eq!(true, errors[0].starts_with("type error. condition type is"), "actual message: `{}`", errors[0]);
            assert_eq!(true, errors[1].starts_with("type error. right is"), "actual message: `{}`", errors[1]);
        } else {
            assert!(false);
        }
    }
}
