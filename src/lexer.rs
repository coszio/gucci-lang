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

    #[test]
    fn test_operators() {
        let src = "
let x = 1 + 3 * 5;
let y = 6 + 3;
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

            let tokens = tokens.unwrap();

            println!("{:?}", errs);
            assert!(errs.is_empty());

            assert_eq!(tokens.len(), 16);

            assert_eq!(tokens[0], (Token::Let, 1..4));
            assert_eq!(tokens[1], (Token::Ident("x".to_string()), 5..6));
            assert_eq!(tokens[2], (Token::Op(Op::Assign), 7..8));
            assert_eq!(tokens[3], (Token::Int("1".to_string()), 9..10));
            assert_eq!(tokens[4], (Token::Op(Op::Add), 11..12));
            assert_eq!(tokens[5], (Token::Int("3".to_string()), 13..14));
            assert_eq!(tokens[6], (Token::Op(Op::Mul), 15..16));
            assert_eq!(tokens[7], (Token::Int("5".to_string()), 17..18));
            assert_eq!(tokens[8], (Token::Ctrl(';'), 18..19));

            assert_eq!(tokens[9], (Token::Let, 20..23));
            assert_eq!(tokens[10], (Token::Ident("y".to_string()), 24..25));
            assert_eq!(tokens[11], (Token::Op(Op::Assign), 26..27));
            assert_eq!(tokens[12], (Token::Int("6".to_string()), 28..29));
            assert_eq!(tokens[13], (Token::Op(Op::Add), 30..31));
            assert_eq!(tokens[14], (Token::Int("3".to_string()), 32..33));
            assert_eq!(tokens[15], (Token::Ctrl(';'), 33..34));
            
    }

    #[test]
    fn test_keywords() {
        let src = "
if x > 9 {
    y = 1;
} else {
    y = 0;
}
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

            let tokens = tokens.unwrap();

            println!("{:?}", errs);
            assert!(errs.is_empty());

            assert_eq!(tokens.len(), 17);

            assert_eq!(tokens[0], (Token::If, 1..3));
            assert_eq!(tokens[1], (Token::Ident("x".to_string()), 4..5));
            assert_eq!(tokens[2], (Token::Op(Op::Gt), 6..7));
            assert_eq!(tokens[3], (Token::Int("9".to_string()), 8..9));
            assert_eq!(tokens[4], (Token::Ctrl('{'), 10..11));

            assert_eq!(tokens[5], (Token::Ident("y".to_string()), 16..17));
            assert_eq!(tokens[6], (Token::Op(Op::Assign), 18..19));
            assert_eq!(tokens[7], (Token::Int("1".to_string()), 20..21));
            assert_eq!(tokens[8], (Token::Ctrl(';'), 21..22));

            assert_eq!(tokens[9], (Token::Ctrl('}'), 23..24));
            assert_eq!(tokens[10], (Token::Else, 25..29));
            assert_eq!(tokens[11], (Token::Ctrl('{'), 30..31));

            assert_eq!(tokens[12], (Token::Ident("y".to_string()), 36..37));
            assert_eq!(tokens[13], (Token::Op(Op::Assign), 38..39));
            assert_eq!(tokens[14], (Token::Int("0".to_string()), 40..41));
            assert_eq!(tokens[15], (Token::Ctrl(';'), 41..42));
            assert_eq!(tokens[16], (Token::Ctrl('}'), 43..44));
            
    }

    #[test]
    fn test_idents() {
        let src = "
let x1 = 20;
let x2 = x1;
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

            let tokens = tokens.unwrap();

            println!("{:?}", errs);
            assert!(errs.is_empty());

            assert_eq!(tokens.len(), 10);

            assert_eq!(tokens[0], (Token::Let, 1..4));
            assert_eq!(tokens[1], (Token::Ident("x1".to_string()), 5..7));
            assert_eq!(tokens[2], (Token::Op(Op::Assign), 8..9));
            assert_eq!(tokens[3], (Token::Int("20".to_string()), 10..12));
            assert_eq!(tokens[4], (Token::Ctrl(';'), 12..13));

            assert_eq!(tokens[5], (Token::Let, 14..17));
            assert_eq!(tokens[6], (Token::Ident("x2".to_string()), 18..20));
            assert_eq!(tokens[7], (Token::Op(Op::Assign), 21..22));
            assert_eq!(tokens[8], (Token::Ident("x1".to_string()), 23..25));
            assert_eq!(tokens[9], (Token::Ctrl(';'), 25..26));
            
    }

    #[test]
    fn test_ctrl_chars() {
        let src = "
let x = (2 + 3) * 2;
let y[2];
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

            let tokens = tokens.unwrap();

            println!("{:?}", errs);
            assert!(errs.is_empty());

            assert_eq!(tokens.len(), 17);

            assert_eq!(tokens[0], (Token::Let, 1..4));
            assert_eq!(tokens[1], (Token::Ident("x".to_string()), 5..6));
            assert_eq!(tokens[2], (Token::Op(Op::Assign), 7..8));
            assert_eq!(tokens[3], (Token::Ctrl('('), 9..10));
            assert_eq!(tokens[4], (Token::Int("2".to_string()), 10..11));
            assert_eq!(tokens[5], (Token::Op(Op::Add), 12..13));
            assert_eq!(tokens[6], (Token::Int("3".to_string()), 14..15));
            assert_eq!(tokens[7], (Token::Ctrl(')'), 15..16));
            assert_eq!(tokens[8], (Token::Op(Op::Mul), 17..18));
            assert_eq!(tokens[9], (Token::Int("2".to_string()), 19..20));
            assert_eq!(tokens[10], (Token::Ctrl(';'), 20..21));

            assert_eq!(tokens[11], (Token::Let, 22..25));
            assert_eq!(tokens[12], (Token::Ident("y".to_string()), 26..27));
            assert_eq!(tokens[13], (Token::Ctrl('['), 27..28));
            assert_eq!(tokens[14], (Token::Int("2".to_string()), 28..29));
            assert_eq!(tokens[15], (Token::Ctrl(']'), 29..30));
            assert_eq!(tokens[16], (Token::Ctrl(';'), 30..31));
            
    }

    #[test]
    fn test_tuple() {
        let src = "
let x(3, 6);
let y(2, 'a', 3);
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

            let tokens = tokens.unwrap();

            println!("{:?}", errs);
            assert!(errs.is_empty());

            assert_eq!(tokens.len(), 18);

            assert_eq!(tokens[0], (Token::Let, 1..4));
            assert_eq!(tokens[1], (Token::Ident("x".to_string()), 5..6));
            assert_eq!(tokens[2], (Token::Ctrl('('), 6..7));
            assert_eq!(tokens[3], (Token::Int("3".to_string()), 7..8));
            assert_eq!(tokens[4], (Token::Ctrl(','), 8..9));
            assert_eq!(tokens[5], (Token::Int("6".to_string()), 10..11));
            assert_eq!(tokens[6], (Token::Ctrl(')'), 11..12));
            assert_eq!(tokens[7], (Token::Ctrl(';'), 12..13));
            
            assert_eq!(tokens[8], (Token::Let, 14..17));
            assert_eq!(tokens[9], (Token::Ident("y".to_string()), 18..19));
            assert_eq!(tokens[10], (Token::Ctrl('('), 19..20));
            assert_eq!(tokens[11], (Token::Int("2".to_string()), 20..21));
            assert_eq!(tokens[12], (Token::Ctrl(','), 21..22));
            assert_eq!(tokens[13], (Token::Char('a'), 23..26));
            assert_eq!(tokens[14], (Token::Ctrl(','), 26..27));
            assert_eq!(tokens[15], (Token::Int("3".to_string()), 28..29));
            assert_eq!(tokens[16], (Token::Ctrl(')'), 29..30));
            assert_eq!(tokens[17], (Token::Ctrl(';'), 30..31));
            
    }

    #[test]
    fn test_fun() {
        let src = "
fun hola() -> Int {
    5
}
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

            let tokens = tokens.unwrap();

            println!("{:?}", errs);
            assert!(errs.is_empty());

            assert_eq!(tokens.len(), 10);

            assert_eq!(tokens[0], (Token::Fun, 1..4));
            assert_eq!(tokens[1], (Token::Ident("hola".to_string()), 5..9));
            assert_eq!(tokens[2], (Token::Ctrl('('), 9..10));
            assert_eq!(tokens[3], (Token::Ctrl(')'), 10..11));
            assert_eq!(tokens[4], (Token::Op(Op::Sub), 12..13));
            assert_eq!(tokens[5], (Token::Op(Op::Gt), 13..14));
            assert_eq!(tokens[6], (Token::Ident("Int".to_string()), 15..18));
            assert_eq!(tokens[7], (Token::Ctrl('{'), 19..20));

            assert_eq!(tokens[8], (Token::Int("5".to_string()), 25..26));

            assert_eq!(tokens[9], (Token::Ctrl('}'), 27..28));
    }

    #[test]
    fn test_class() {
        let src = "
class hola{
    let x = 3;
}
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

            let tokens = tokens.unwrap();

            println!("{:?}", errs);
            assert!(errs.is_empty());

            assert_eq!(tokens.len(), 9);

            assert_eq!(tokens[0], (Token::Class, 1..6));
            assert_eq!(tokens[1], (Token::Ident("hola".to_string()), 7..11));
            assert_eq!(tokens[2], (Token::Ctrl('{'), 11..12));

            assert_eq!(tokens[3], (Token::Let, 17..20));
            assert_eq!(tokens[4], (Token::Ident("x".to_string()), 21..22));
            assert_eq!(tokens[5], (Token::Op(Op::Assign), 23..24));
            assert_eq!(tokens[6], (Token::Int("3".to_string()), 25..26));
            assert_eq!(tokens[7], (Token::Ctrl(';'), 26..27));

            assert_eq!(tokens[8], (Token::Ctrl('}'), 28..29));
            
    }
}

