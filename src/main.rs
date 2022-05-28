mod compiler;
pub mod utils;
pub mod shared;
mod vm;

use std::{fs, env};
use ariadne::Source;
use chumsky::{Parser, Stream};

use crate::compiler::{lexer, parser, translator, semantics, err_fmt::formatter::Error};


fn main() {
    // let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
    //     .expect("Failed to read file");
    let filename = "examples/simple.gu";
    let src = fs::read_to_string(filename.clone())
        .expect("Failed to read file");

    // Tokenize
    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src.as_str());
    
    // print lexical errors
    lex_errs.iter().for_each(|e| {
        Error::from(e)
            .report(filename.clone())
            .print((filename.clone(), Source::from(&src.clone())))
            .unwrap()
    });
    
    let tokens = tokens.unwrap();

    // Parse
    let len = src.chars().count();
    let (stmts, parse_errs) = parser::parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));
    
    // print parsing errors
    parse_errs.iter().for_each(|e| {
        Error::from(e)
            .report(filename.clone())
            .print((filename.clone(), Source::from(&src.clone())))
            .unwrap()
    });

    // Check types
    let res = semantics::semantic_analysis(stmts.clone().unwrap());

    if let Err(errs) = res {
        errs.iter().for_each(|e| {
            Error::from(e)
                .report(filename.clone())
                .print((filename.clone(), Source::from(&src.clone())))
                .unwrap()
        });
    } else {
        // Translate only if there were no semantic errors
        let res = translator::translate(res.unwrap()).unwrap();
    
        println!("{}", res);
    }



    
}
