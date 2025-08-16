use std::collections::HashSet;

use crate::parser::ast::{Expression, ExternalItem, Parameter, Program, Statement, TypeRef};

#[derive(Debug)]
pub struct Error {
    errors: Vec<String>,
}

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
                types.put(tag_name);
            },
            ExternalItem::VarDecl(declarators) => {
                for (_, declarator) in declarators {
                    global_scope.put(declarator.name.as_str());
                    // var_decl_initializer.push(declarator);
                }
            },
            ExternalItem::FunctionDecl { return_type_dec, name, parameters, body } => {
                functions.put(name);
            },
            _ => {},
        }
    }

    println!("@@@@ check_function types: {:?}, functions: {:?}, global_scope: {:?}", types, functions, &global_scope);

    let results: Vec<Error> = ast.external_items.iter()
        .filter_map(|item| {
            if let ExternalItem::FunctionDecl { return_type_dec, name, parameters, body } = item {
                Some(check_function(
                    &types,
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
    types: &Types,
    functions: &Functions,
    global_scope: &LocalScope,
    return_type_dec: &TypeRef,
    name: &str,
    parameters: &Vec<Parameter>,
    body: &Option<Box<Statement>>,
) -> Vec<Error> {
    let mut results: Vec<Error> = vec![];
    println!("@@@@ check_function ::: return_type_dec: {:?}, name: {:?}, parameters: {:?}", return_type_dec, name, parameters);
    if !types.find(&return_type_dec.type_name()) {
        results.push(Error { errors: vec![format!("return type is not defined: {:?}", return_type_dec)] });
        return results;
    }
    let mut local_scope = LocalScope { parent: Some(&global_scope), entities: HashSet::new() };
    for p in parameters {
        local_scope.put(p.name.as_str());
    }
    println!("@@@@ check_function ::: local_scope: {:?}", local_scope);
    let result: Result<()> = if let Some(stmt) = body {
        match stmt.as_ref() {
            Statement::Return(expression) => todo!(),
            Statement::Break => Ok(()),
            Statement::Continue => Ok(()),
            Statement::VarDecl(items) => todo!(),
            Statement::Block(statements) => todo!(),
            Statement::If { condition, consequence, alternative } => todo!(),
            Statement::Switch { condition, switch_block } => todo!(),
            Statement::While { condition, body } => todo!(),
            Statement::DoWhile { body, condition } => todo!(),
            Statement::For { init, condition, post, body } => todo!(),
            Statement::ExpressionStatement(expression) => todo!(),
        }
    } else {
        Ok(())
    };
    results
}

fn check_expression(scope: &LocalScope, exp: &Expression) -> Result<()> {
    match exp {
        Expression::Int(_) => Ok(()),
        Expression::CharacterLiteral(_) => Ok(()),
        Expression::StringLiteral(_) => Ok(()),
        Expression::Identifier(name) => todo!(),
        Expression::PrefixExpression { operator: _, right } => todo!(),
        Expression::InfixExpression { operator: _, left, right } => todo!(),
        Expression::PostfixExpression { operator: _, left } => todo!(),
        Expression::FunctionCallExpression { function_name, arguments } => todo!(),
        Expression::InitializerExpression { elements } => todo!(),
        Expression::IndexExpression { left, index } => todo!(),
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
    fn test_1() {
        // given
        let input = "
int x = 10;
struct point {
    int x;
    int y;
};
struct rect foo(point p) {
    struct rect r = {p.x, p.y, 10, 10};
    return r;
}
";
        let mut parser = Parser::new(Lexer::new(input));
        let ast = parser.parse_program();

        // when
        let result = check_scope(&ast);

        // then
        if let Some(Error{ errors }) = result.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(true, errors[0].starts_with("return type is not defined"));
        } else {
            assert!(false);
        }
    }
}
