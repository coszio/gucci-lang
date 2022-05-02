mod lexer;
mod parser;
mod formatter;
mod directory;
mod semantics;
mod semantic_cube;

use std::{fs, env};
use chumsky::{Parser, Stream};


fn main() {
    // let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
    //     .expect("Failed to read file");

    let src = fs::read_to_string("examples/simple.gu")
        .expect("Failed to read file");

    println!("{:?}", src.as_str());

    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src.as_str());
    
    if lex_errs.len() > 0 {
        println!("lexing errors found: {:?}", lex_errs);
    }
    
    let tokens = tokens.unwrap();

    println!("Tokens: {:?}", tokens);

    let len = src.chars().count();
    
    let (stmts, parse_errs) = parser::parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));
    
    println!("parse errors: {:?}", parse_errs);

    stmts
        .unwrap()
        .iter()
        .for_each(|stmt| {
            println!("{:?}", stmt);
        });

}
