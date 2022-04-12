use std::fmt::Display;

use chumsky::{
    prelude::{filter, just, Simple, one_of, take_until, skip_then_retry_until},
    text::{self, TextParser},
    Parser
};

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
    Not,
    Assign,
    Dot,
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Void,
    Ident(String),
    Int(String),
    Float(String),
    Op(Op),
    Ctrl(char),
    Str(String),
    Char(char),
    Bool(bool),
    Fun,
    Class,
    Interface,
    Inherits,
    Implements,
    If,
    Else,
    Let,
    Return,
    For,
    In,
    While,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Void => write!(f, "void"),
            Token::Ident(id) => write!(f, "{}", id),
            Token::Int(i) => write!(f, "{}", i),
            Token::Float(fl) => write!(f, "{}", fl),
            Token::Str(s) => write!(f, "{}", s),
            Token::Char(c) => write!(f, "{}", c),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Ctrl(ctrl) => write!(f, "{}", ctrl),
            Token::Op(op) => write!(f, "{}", op),
            Token::Fun => write!(f, "function"),
            Token::Class => write!(f, "class"),
            Token::Interface => write!(f, "interface"),
            Token::Inherits => write!(f, "inherits"),
            Token::Implements => write!(f, "implements"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Let => write!(f, "let"),
            Token::Return => write!(f, "return"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::While => write!(f, "while"),
        }
    }
}


fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {

    let keyword_or_id = text::ident().map(|s: String| {
      match s.as_str() {
        "void"      => Token::Void,
        "fun"       => Token::Fun,
        "class"     => Token::Class,
        "interface" => Token::Interface,
        "inherits"  => Token::Inherits,
        "implements"=> Token::Implements,
        "if"        => Token::If,
        "else"      => Token::Else,
        "let"       => Token::Let,
        "return"    => Token::Return,
        "for"       => Token::For,
        "in"        => Token::In,
        "while"     => Token::While,
        _           => Token::Ident(s),
      }
    });

    let int_ = text::int(10).map(Token::Int);

    let float_ = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)))
        .collect::<String>()
        .map(Token::Float);

    let str_ = filter(|c| *c != '"')
        .repeated()
        .delimited_by(just('"'), just('"'))
        .collect::<String>()
        .map(Token::Str);

    let char_ = filter(|c| *c != '\'')
        .delimited_by(just('\''), just('\''))
        .map(Token::Char);

    let ctrl = one_of("()[]{};,")
        .map(Token::Ctrl);

    let comment = just("//")
        .then(take_until(just('\n')))
        .padded();

    let op = just("+").to(Token::Op(Op::Add))
        .or(just("-") .to(Token::Op(Op::Sub)))
        .or(just("*") .to(Token::Op(Op::Mul)))
        .or(just("/") .to(Token::Op(Op::Div)))
        .or(just(".") .to(Token::Op(Op::Dot)))
        .or(just("<") .to(Token::Op(Op::Lt )))
        .or(just(">") .to(Token::Op(Op::Gt )))
        .or(just("<=").to(Token::Op(Op::Lte)))
        .or(just(">=").to(Token::Op(Op::Gte)))
        .or(just("==").to(Token::Op(Op::Eq )))
        .or(just("!=").to(Token::Op(Op::Ne )))
        .or(just("&&").to(Token::Op(Op::And)))
        .or(just("||").to(Token::Op(Op::Or )))
        .or(just("!") .to(Token::Op(Op::Not)))
        .or(just("=") .to(Token::Op(Op::Assign)));

    let token = int_
        .or(float_)
        .or(str_)
        .or(char_)
        .or(ctrl)
        .or(op)
        .or(keyword_or_id)
        .recover_with(skip_then_retry_until([]))
        .padded_by(comment.repeated())
        .map_with_span(|tok, span| (tok, span))
        .padded()
        .repeated();

    token
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer() {
        let src = "
let x = 1;
let z = x + y;
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

            let tokens = tokens.unwrap();

            println!("{:?}", errs);
            assert!(errs.is_empty());

            assert_eq!(tokens.len(), 12);

            assert_eq!(tokens[0], (Token::Let, 1..4));
            assert_eq!(tokens[1], (Token::Ident("x".to_string()), 5..6));
            assert_eq!(tokens[2], (Token::Op(Op::Assign), 7..8));
            assert_eq!(tokens[3], (Token::Int("1".to_string()), 9..10));
            assert_eq!(tokens[4], (Token::Ctrl(';'), 10..11));

            assert_eq!(tokens[5], (Token::Let, 12..15));
            assert_eq!(tokens[6], (Token::Ident("z".to_string()), 16..17));
            assert_eq!(tokens[7], (Token::Op(Op::Assign), 18..19));
            assert_eq!(tokens[8], (Token::Ident("x".to_string()), 20..21));
            assert_eq!(tokens[9], (Token::Op(Op::Add), 22..23));
            assert_eq!(tokens[10], (Token::Ident("y".to_string()), 24..25));
            assert_eq!(tokens[11], (Token::Ctrl(';'), 25..26));
            
    }
}

