mod lexer;
mod parser;
mod formatter;
mod directory;
mod semantics;
mod semantic_cube;
mod translator;

use std::{fs, env};
use chumsky::{Parser, Stream};


fn main() {
    // let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
    //     .expect("Failed to read file");

    // Tokenize
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
    
    // Parse
    let (stmts, parse_errs) = parser::parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));
    
    println!("parse errors: {:?}", parse_errs);

    // stmts.clone()
    //     .unwrap()
    //     .iter()
    //     .for_each(|stmt| {
    //         println!("{:?}", stmt);
    //     });

    // Check types
    let res = semantics::semantic_analysis(stmts.clone().unwrap());

    if let Err(errs) = res {
        println!("semantic errors: {:?}", errs);
    } else {
        
        // Translate only if there were no semantic errors
        let res = translator::translate(stmts.unwrap()).unwrap();
    
        println!("{}", res);
    }



    
}
