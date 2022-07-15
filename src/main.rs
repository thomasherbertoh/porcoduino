pub mod astnode;
pub mod lexer;
pub mod parser;
pub mod tokens;

use lexer::Lexer;
use parser::Parser;
use std::{env, fs};
use tokens::TokenType;

fn main() {
    let path = env::args().nth(1).expect("Usage: ./porcoduino [FILE-PATH]");

    let program = fs::read_to_string(path.clone())
        .expect(&format!("Unable to read the file at path '{path}'"));

    let mut lexer = Lexer::new(program);
    let tokens = lexer.lex();

    for token in tokens.clone() {
        print!("{:?} ", token.t_type);
        if token.t_type == TokenType::End {
            print!("\n");
        }
    }

    let _parser = Parser::new(&tokens);
}
