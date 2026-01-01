use crate::{parser::ast::Program, sema::env::Env};

mod scope_checker;
mod semantic_checker;
mod type_checker;
mod env;

pub fn semantic_analyze(ast: Program) {
    let env = &mut Env::new();
    let _env = scope_checker::check_scope(env, &ast);
    let _ = semantic_checker::check_semantic(&ast);
    let _ = type_checker::check_type(env, &ast);
}
