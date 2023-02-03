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
        .unwrap_or_else(|_| panic!("Unable to read the file at path '{path}'"));

    let mut lexer = Lexer::new(program);
    let tokens = lexer.lex();

    /*
    for token in tokens.clone() {
        print!("{:?} ", token.t_type);
        if matches!(
            token.t_type,
            TokenType::End | TokenType::StartBlock | TokenType::EndBlock
        ) {
            print!("\n");
        }
    }
    */

    let parser = Parser::new(&tokens);

    let nodes = parser.parse(0, tokens.len() - 1);

    /*
    for node in nodes.clone() {
        println!("node = {:#?}", node);
    }
    */

    println!("Compilation took {} seconds", start.elapsed().as_secs_f32());

    start = Instant::now();

    let mut evaluator = Evaluator::new(&nodes);

    evaluator.evaluate();

    println!("Execution took {} seconds", start.elapsed().as_secs_f32());

    let output = evaluator.get_vars();

    for (i, scope) in output.iter().enumerate() {
        let mut scope_vec = scope.iter().collect::<Vec<_>>();
        scope_vec.sort_by(|a, b| a.0.cmp(b.0));
        println!("Scope {}: {{", i);
        for variable in scope_vec {
            println!("  {} = {:?}", variable.0, variable.1);
        }
        println!("}}");
    }
}
