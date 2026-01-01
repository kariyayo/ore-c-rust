mod lexer;
mod parser;
pub mod repl;
mod sema;

pub fn create_parser(input: &str) -> parser::Parser {
    // compiler::compile(input);
    let l = lexer::Lexer::new(input);
    let p = parser::Parser::new(l);
    return p;
}

pub fn compile(input: &str) {
    let mut parser = create_parser(input);
    let ast = parser.parse_program();
    sema::semantic_analyze(ast)
    // TODO:
}
