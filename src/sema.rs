use crate::parser::ast::Program;

mod env;
mod scope_checker;
mod semantic_checker;
mod type_checker;

pub fn semantic_analyze(ast: Program) {
    let env = scope_checker::check_scope(&ast).unwrap();
    let _ = semantic_checker::check_semantic(&ast);
    let _ = type_checker::check_type(&ast, &env);
}
