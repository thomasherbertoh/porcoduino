pub mod astnode;
pub mod evaluator;
pub mod lexer;
pub mod parser;
pub mod tokens;

use evaluator::Evaluator;
use lexer::Lexer;
use parser::Parser;
use std::{env, fs, time::Instant};

fn main() {
    let mut start = Instant::now();

    let path = env::args().nth(1).expect("Usage: ./porcoduino [FILE-PATH]");

    let program = fs::read_to_string(path.clone())
        .expect(&format!("Unable to read the file at path '{path}'"));

    let mut lexer = Lexer::new(program);
    let tokens = lexer.lex();

    /*
    for token in tokens.clone() {
        print!("{:?} ", token.t_type);
        if token.t_type == TokenType::End {
            print!("\n");
        }
    }
    */

    let parser = Parser::new(&tokens);

    let nodes = parser.parse();

    /*
    for node in nodes.clone() {
        println!("node = {:#?}", node);
    }
    */

    println!("Compilation took {} seconds", start.elapsed().as_secs_f32());

    start = Instant::now();

    let mut evaluator = Evaluator::new(&nodes);

    let results = evaluator.evaluate();

    println!("Execution took {} seconds", start.elapsed().as_secs_f32());

    println!("{:?}", results);
}
