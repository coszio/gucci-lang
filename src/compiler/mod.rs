use std::{fs, io::Write};

use ariadne::Source;
use chumsky::{Stream, Parser};

use self::err_fmt::formatter::Error;

pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod semantics;
pub(crate) mod translator;
pub(crate) mod err_fmt;

pub fn compile(filename: &str) -> Option<String> {

    if !filename.ends_with(".gu") {
        panic!("Gucci files should have the extension .gu");
    }

    let src = fs::read_to_string(filename.clone())
        .expect("Failed to read file");

    // Tokenize
    let (tokens, lex_errs) = lexer::lexer().parse_recovery(src.as_str());
    
    // print lexical errors
    for e in lex_errs.iter() {
        Error::from(e)
            .report(filename.clone())
            .print((filename.clone(), Source::from(&src.clone())))
            .unwrap()
    };
    
    let tokens = tokens.unwrap();

    // Parse
    let len = src.chars().count();
    let stream = Stream::from_iter(len..len + 1, tokens.into_iter());
    let (stmts, parse_errs) = parser::parser().parse_recovery(stream);
    
    // print parsing errors
    for e in parse_errs.iter() {
        Error::from(e)
            .report(filename.clone())
            .print((filename.clone(), Source::from(&src.clone())))
            .unwrap()
    };

    // Check types
    let res = semantics::semantic_analysis(stmts.clone().unwrap());

    if let Err(errs) = res {
        for e in errs.iter() {
            Error::from(e)
                .report(filename.clone())
                .print((filename.clone(), Source::from(&src.clone())))
                .unwrap()
        };
        None
    } else {
        // Translate only if there were no semantic errors
        let res = translator::translate(res.unwrap()).unwrap();
    
        // write to file
        let obj_filename = format!("{}.bs", filename);
        let mut file = fs::File::create(&obj_filename).unwrap();
        write!(file, "{}", res.to_string()).unwrap();
        Some(obj_filename)
    }
  }
  