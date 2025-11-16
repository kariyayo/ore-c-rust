use crate::parser::ast::Program;

mod scope_checker;
mod type_checker;

pub fn semantic_analyze(ast: Program) {
    let env = scope_checker::check_scope(&ast);
    type_checker::check_type(&ast);
}
