pub mod repl;
mod lexer;
mod parser;

pub fn create_parser(input: &str) -> parser::Parser {
    let l = lexer::Lexer::new(input);
    let p = parser::Parser::new(l);
    return p;
}
