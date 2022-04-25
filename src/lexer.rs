use std::fmt::Display;

use chumsky::{
    prelude::{filter, just, one_of, skip_then_retry_until, take_until, Simple},
    text::{self, TextParser},
    Parser,
};

pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    Type(String),
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
    This,
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
            Token::Type(t) => write!(f, "{}", t),
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
            Token::This => write!(f, "this")
        }
    }
}

pub fn lexer() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    let keyword_or_id = text::ident().map(|s: String| match s.as_str() {
        "void" => Token::Void,
        "fun" => Token::Fun,
        "class" => Token::Class,
        "interface" => Token::Interface,
        "inherits" => Token::Inherits,
        "implements" => Token::Implements,
        "if" => Token::If,
        "else" => Token::Else,
        "let" => Token::Let,
        "return" => Token::Return,
        "for" => Token::For,
        "in" => Token::In,
        "while" => Token::While,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        "int" => Token::Type("int".to_string()),
        "float" => Token::Type("float".to_string()),
        "bool" => Token::Type("bool".to_string()),
        "string" => Token::Type("string".to_string()),
        "char" => Token::Type("char".to_string()),
        "this" => Token::This,
        _ => Token::Ident(s),
    });

    let float_ = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)))
        .collect::<String>()
        .map(Token::Float);

    let int_ = text::int(10).map(Token::Int);

    let str_ = filter(|c| *c != '"')
        .repeated()
        .delimited_by(just('"'), just('"'))
        .collect::<String>()
        .map(Token::Str);

    let char_ = filter(|c| *c != '\'')
        .delimited_by(just('\''), just('\''))
        .map(Token::Char);

    let ctrl = one_of("()[]{},;:").map(Token::Ctrl);

    let comment = just("//").then(take_until(just('\n'))).padded();

    let op = just("<=")
        .to(Token::Op(Op::Lte))
        .or(just(">=").to(Token::Op(Op::Gte)))
        .or(just("==").to(Token::Op(Op::Eq)))
        .or(just("!=").to(Token::Op(Op::Ne)))
        .or(just("&&").to(Token::Op(Op::And)))
        .or(just("||").to(Token::Op(Op::Or)))
        .or(just("+").to(Token::Op(Op::Add)))
        .or(just("-").to(Token::Op(Op::Sub)))
        .or(just("*").to(Token::Op(Op::Mul)))
        .or(just("/").to(Token::Op(Op::Div)))
        .or(just(".").to(Token::Op(Op::Dot)))
        .or(just("<").to(Token::Op(Op::Lt)))
        .or(just(">").to(Token::Op(Op::Gt)))
        .or(just("!").to(Token::Op(Op::Not)))
        .or(just("=").to(Token::Op(Op::Assign)));

    let token = float_
        .or(int_)
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

        assert_eq!(tokens.len(), 12);
    }

    #[test]
    fn test_operators() {
        let src = "
+ - * / < > <= >= == != && || ! = .
";
        let (tokens, errs) = lexer().parse_recovery(src);

        let tokens = tokens.unwrap();

        println!("{:?}", errs);
        assert!(errs.is_empty());

        assert_eq!(tokens[0], (Token::Op(Op::Add), 1..2));
        assert_eq!(tokens[1], (Token::Op(Op::Sub), 3..4));
        assert_eq!(tokens[2], (Token::Op(Op::Mul), 5..6));
        assert_eq!(tokens[3], (Token::Op(Op::Div), 7..8));
        assert_eq!(tokens[4], (Token::Op(Op::Lt), 9..10));
        assert_eq!(tokens[5], (Token::Op(Op::Gt), 11..12));
        assert_eq!(tokens[6], (Token::Op(Op::Lte), 13..15));
        assert_eq!(tokens[7], (Token::Op(Op::Gte), 16..18));
        assert_eq!(tokens[8], (Token::Op(Op::Eq), 19..21));
        assert_eq!(tokens[9], (Token::Op(Op::Ne), 22..24));
        assert_eq!(tokens[10], (Token::Op(Op::And), 25..27));
        assert_eq!(tokens[11], (Token::Op(Op::Or), 28..30));
        assert_eq!(tokens[12], (Token::Op(Op::Not), 31..32));
        assert_eq!(tokens[13], (Token::Op(Op::Assign), 33..34));
        assert_eq!(tokens[14], (Token::Op(Op::Dot), 35..36));

        assert_eq!(tokens.len(), 15);
    }

    #[test]
    fn test_keywords() {
        let src = "
if else while for in return fun let class interface inherits implements this
";
        let (tokens, errs) = lexer().parse_recovery(src);

        let tokens = tokens.unwrap();

        println!("{:?}", errs);
        assert!(errs.is_empty());

        assert_eq!(tokens[0], (Token::If, 1..3));
        assert_eq!(tokens[1], (Token::Else, 4..8));
        assert_eq!(tokens[2], (Token::While, 9..14));
        assert_eq!(tokens[3], (Token::For, 15..18));
        assert_eq!(tokens[4], (Token::In, 19..21));
        assert_eq!(tokens[5], (Token::Return, 22..28));
        assert_eq!(tokens[6], (Token::Fun, 29..32));
        assert_eq!(tokens[7], (Token::Let, 33..36));
        assert_eq!(tokens[8], (Token::Class, 37..42));
        assert_eq!(tokens[9], (Token::Interface, 43..52));
        assert_eq!(tokens[10], (Token::Inherits, 53..61));
        assert_eq!(tokens[11], (Token::Implements, 62..72));
        assert_eq!(tokens[12], (Token::This, 73..77));

        assert_eq!(tokens.len(), 13);
    }

    #[test]
    fn test_idents() {
        let src = "
normal __dunder__ camelCase PascalCase snake_case CONSTANT_CASE numbered123 _123 _123_
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

        let tokens = tokens.unwrap();

        println!("{:?}", errs);
        assert!(errs.is_empty());

        assert_eq!(tokens[0], (Token::Ident("normal".to_string()), 1..7));
        assert_eq!(tokens[1], (Token::Ident("__dunder__".to_string()), 8..18));
        assert_eq!(tokens[2], (Token::Ident("camelCase".to_string()), 19..28));
        assert_eq!(tokens[3], (Token::Ident("PascalCase".to_string()), 29..39));
        assert_eq!(tokens[4], (Token::Ident("snake_case".to_string()), 40..50));
        assert_eq!(
            tokens[5],
            (Token::Ident("CONSTANT_CASE".to_string()), 51..64)
        );
        assert_eq!(tokens[6], (Token::Ident("numbered123".to_string()), 65..76));
        assert_eq!(tokens[7], (Token::Ident("_123".to_string()), 77..81));
        assert_eq!(tokens[8], (Token::Ident("_123_".to_string()), 82..87));

        assert_eq!(tokens.len(), 9);
    }

    #[test]
    fn test_ctrl() {
        let src = "
[ ] ( ) { } , ; :
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

        let tokens = tokens.unwrap();

        println!("{:?}", errs);
        assert!(errs.is_empty());

        assert_eq!(tokens[0], (Token::Ctrl('['), 1..2));
        assert_eq!(tokens[1], (Token::Ctrl(']'), 3..4));
        assert_eq!(tokens[2], (Token::Ctrl('('), 5..6));
        assert_eq!(tokens[3], (Token::Ctrl(')'), 7..8));
        assert_eq!(tokens[4], (Token::Ctrl('{'), 9..10));
        assert_eq!(tokens[5], (Token::Ctrl('}'), 11..12));
        assert_eq!(tokens[6], (Token::Ctrl(','), 13..14));
        assert_eq!(tokens[7], (Token::Ctrl(';'), 15..16));
        assert_eq!(tokens[8], (Token::Ctrl(':'), 17..18));

        assert_eq!(tokens.len(), 9);
    }

    #[test]
    fn test_int() {
        let src = "
2 35 254 1000 45000 0
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

        let tokens = tokens.unwrap();

        println!("{:?}", errs);
        assert!(errs.is_empty());

        assert_eq!(tokens[0], (Token::Int("2".to_string()), 1..2));
        assert_eq!(tokens[1], (Token::Int("35".to_string()), 3..5));
        assert_eq!(tokens[2], (Token::Int("254".to_string()), 6..9));
        assert_eq!(tokens[3], (Token::Int("1000".to_string()), 10..14));
        assert_eq!(tokens[4], (Token::Int("45000".to_string()), 15..20));
        assert_eq!(tokens[5], (Token::Int("0".to_string()), 21..22));

        assert_eq!(tokens.len(), 6);
    }
    
    #[test]
    fn test_float() {
        let src = "
3.5 4.01 100.2503 0.1
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

        let tokens = tokens.unwrap();

        println!("{:?}", errs);
        assert!(errs.is_empty());

        assert_eq!(tokens[0], (Token::Float("3.5".to_string()), 1..4));
        assert_eq!(tokens[1], (Token::Float("4.01".to_string()), 5..9));
        assert_eq!(tokens[2], (Token::Float("100.2503".to_string()), 10..18));
        assert_eq!(tokens[3], (Token::Float("0.1".to_string()), 19..22));

        assert_eq!(tokens.len(), 4);
    }

    #[test]
    fn test_bool() {
        let src = "
true false
        ";
        let (tokens, errs) = lexer().parse_recovery(src);

        let tokens = tokens.unwrap();

        println!("{:?}", errs);
        assert!(errs.is_empty());

        assert_eq!(tokens[0], (Token::Bool(true), 1..5));
        assert_eq!(tokens[1], (Token::Bool(false), 6..11));

        assert_eq!(tokens.len(), 2);
    }
}
