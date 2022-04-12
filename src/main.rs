mod lexer;
mod parser;
mod formatter;

use std::{fs, env};
use chumsky::Parser;


fn main() {
    // let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
    //     .expect("Failed to read file");

    // let (tokens, mut errs) = parser().parse_recovery(src.as_str());
}
