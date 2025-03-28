use crate::lexer;
use std::io;
use std::io::prelude::*;

const PROMPT: &str = ">> ";

pub fn start() -> io::Result<()> {
    print!("{}", PROMPT);
    io::stdout().flush()?;

    let stdin = io::stdin();
    for line_result in stdin.lock().lines() {
        let line = line_result?;
        let mut l = lexer::Lexer::new(&line);
        loop {
            let token = l.next_token();
            if token.token_type == lexer::token::TokenType::Eof {
                break;
            }
            println!("{:?}", token);
        }

        print!("{}", PROMPT);
        io::stdout().flush()?;
    }
    Ok(())
}
