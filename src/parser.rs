use std::fmt::Display;

use chumsky::{
    prelude::{filter, filter_map, just, one_of, skip_then_retry_until, take_until, recursive, Simple},
    text::{self, TextParser},
    Parser, select, recursive,
};

use crate::lexer::{Token, Span, lexer, Op};

type Spanned<T> = (T, Span);

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Declaration(Decl),
    Assignment {
        ident: String, 
        expr: Spanned<Expr>,
    },
    // Condition(Spanned<Cond>),
    Loop(Loop),
    Expression(Spanned<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Let { 
        id: String,
        type_: Type,
        value: Option<Expr>,
    },
    Fun {
        id: String,
        args: Vec<ArgDecl>,
        ret_type: Type,
        body: Block,
    },
}


// pub enum Cond {
//     If(Spanned<Expr>, Vec<Stmt>),
//     IfElse(Expr, Vec<Stmt>, Vec<Stmt>),
// }

#[derive(Debug, Clone, PartialEq)]

pub enum Loop {
    While(Spanned<Expr>, Vec<Stmt>),
}

#[derive(Debug, Clone, PartialEq)]

pub enum Expr {
    Call {
        ident: String, 
        args: Vec<Spanned<Self>>,
    },
    Binary {
        lhs: Box<Spanned<Self>>, 
        op: BinOp, 
        rhs: Box<Spanned<Self>>,
    },
    Unary { 
        op: UnOp,
        rhs: Box<Spanned<Self>>,
    },
    Constant(Literal),
    Ident(String),
    Array(Vec<Spanned<Self>>),
}

#[derive(Debug, Clone, PartialEq)]

pub enum BinOp {
    //// Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    // Mod,
    //// Comparison
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    //// Boolean
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]

pub enum UnOp {
    Not,
    Minus,
}

#[derive(Debug, Clone, PartialEq)]

pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    //// Primitive
    Int,
    Float,
    Bool,
    Char,

    //// Compound
    Array(Box<Self>),
    Tuple(Vec<Box<Self>>),
    String,

    //// Custom
    Ident(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArgDecl {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret_type: Option<Type>,
}

pub struct ClassDecl {
    pub name: String,
    pub parent: Option<String>,
    pub interface: Option<String>,
    pub body: Block,
}

pub struct InterfaceDecl {
    pub name: String,
    pub body: Block,
}


fn parser() -> impl Parser<Token, Vec<Spanned<Stmt>>, Error = Simple<Token>> {
    let ident = select! { Token::Ident(name) => name.clone() }.labelled("identifier");
    
    let expr = recursive(|expr| {
        let ident = ident.clone()
            .map(|name| Expr::Ident(name));
            
        let number = filter_map(|span: Span, tok| {
            match tok {
                Token::Int(i) => Ok(Expr::Constant(Literal::Int(i.parse().unwrap()))),
                Token::Float(f) => Ok(Expr::Constant(Literal::Float(f.parse().unwrap()))),
                _ => Err(Simple::custom(span, "Not a number, expected int or float")),
            }
        }).labelled("number");


        let literal = select! {
            Token::Bool(b) => Expr::Constant(Literal::Bool(b)),
            Token::Char(c) => Expr::Constant(Literal::Char(c)),
            Token::Str(s) => Expr::Constant(Literal::String(s)),
        };
        
        let literal = literal
            .or(number)
            .labelled("literal");

        let item = expr.clone()
            .map_with_span(|item, span: Span| (item, span))
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing()
            .labelled("item");
        
        let array = item.clone()
            // .repeated()
            .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
            .map(|items| Expr::Array(items))
            .labelled("array");

        // let args = item
        //         .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        //         .map_with_span(|(arg, span)| (arg, span))
        //         .repeated()
        //         .foldl(|mut acc, (arg, span)| {
        //             let span = acc.1.start..arg.1.end;
        //             acc.push((args, span));
        //             acc
        //         });

        // let fun_call = ident
        //     .then(item
        //         .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
        //         .map_with_span(|(args, span)| (args, span))
        //         .repeated()
        //     )
        //     .foldl(| |);

        literal
            .or(array)
            .or(ident)
            .labelled("expr")

    });
    
    // let expr = recursive(|expr| {
    // });

    let type_ = recursive(|type_| {
        let simple = select! { 
            Token::Type(t) => match t.as_str() {
                "int" => Type::Int,
                "float" => Type::Float,
                "bool" => Type::Bool,
                "char" => Type::Char,
                "string" => Type::String,
                _ => Type::Ident(t)
            }
          }.labelled("type");
        
        let array = type_.clone()
            .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
            .map(|t| Type::Array(Box::new(t)));

        // let tuple = type_.clone()
        //     .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')));

        simple
            .or(array)
            // .or(tuple)
            // .map_with_span(|t, span| (t, span))
    });
        
    // A let expression
    let let_decl = just(Token::Let)
        .ignore_then(ident)
        .then_ignore(just(Token::Ctrl(':')))
        .then(type_);
        
    let let_assign = let_decl.clone()
        .then_ignore(just(Token::Op(Op::Assign)))
        .then(expr);
        
    let let_ = let_assign
        .map(|((id, type_), value)| Stmt::Declaration(Decl::Let { id, type_, value: Some(value) }))
        .or(let_decl
            .map(|(id, type_)| Stmt::Declaration(Decl::Let { id, type_, value: None })));
    
    let decl = let_;

    let stmt = decl
        .then_ignore(just(Token::Ctrl(';')));

    stmt.recover_with(skip_then_retry_until([]))
        .map_with_span(|tok, span: Span| (tok, span))
        .repeated()

}

#[cfg(test)]
mod tests {

    use chumsky::Stream;

    use super::*;

    fn parse_from(input: &str) -> Vec<Spanned<Stmt>> {
        let (tokens, lex_errs) = lexer().parse_recovery(input);
        assert!(lex_errs.is_empty());
        let tokens = tokens.unwrap();

        let len = input.chars().count();

        let (stmts, parse_errs) = parser().parse_recovery(Stream::from_iter(len..len + 1, tokens.into_iter()));
        
        println!("{:?}", parse_errs);

        stmts.unwrap()
    }

    #[test]
    fn test_var_decl() {
        let src = "
let a: int;
let b: float;
let c: bool;
let d: char;
let e: string;
let f: [int];
";

        let stmts = parse_from(src);

        assert_eq!(
            stmts[0],
            (
                Stmt::Declaration(Decl::Let {
                    id: "a".to_string(),
                    type_: Type::Int,
                    value: None
                }),
                1..12
            ) 
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Declaration(Decl::Let {
                    id: "b".to_string(),
                    type_: Type::Float,
                    value: None
                }),
                13..26
            ) 
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Declaration(Decl::Let {
                    id: "c".to_string(),
                    type_: Type::Bool,
                    value: None
                }),
                27..39
            ) 
        );

        assert_eq!(
            stmts[3],
            (
                Stmt::Declaration(Decl::Let {
                    id: "d".to_string(),
                    type_: Type::Char,
                    value: None
                }),
                40..52
            ) 
        );

        assert_eq!(
            stmts[4],
            (
                Stmt::Declaration(Decl::Let {
                    id: "e".to_string(),
                    type_: Type::String,
                    value: None
                }),
                53..67
            ) 
        );

        assert_eq!(
            stmts[5],
            (
                Stmt::Declaration(Decl::Let {
                    id: "f".to_string(),
                    type_: Type::Array(Box::new(Type::Int)),
                    value: None
                }),
                68..81
            ) 
        );
    }

    #[test]
    fn test_fun_decl() {
        let src = "
fun foo() {
    return 1;
}

fun bar(a: int, b: float): float {
    return a + b;
}

fun baz(a: int, b: float) {
}";
        todo!();
    }

    #[test]
    fn test_class_decl() {
        let src = "
class Foo {
    fun foo() {
        return 1;
    }
}

class Bar extends Foo {
    let a: int = 1;
}

class Baz implements Bal {
    fun faz() {
        return 2;
    }
}";

        todo!();
    }

    #[test]
    fn test_block() {
        let src = "
{
    let a: int = 1;
    let b: int = 2;
    return a + b;
}

{
    let a: int = 1 + 2;
    a
}
{
    let a: int = 10;
    let b: int = 20;
    ";

        todo!();
    }

    #[test]
    fn test_cond() {
        let src = "
if (a == 1) {
    return 1;
}

if (a == 1) {
    return 1;
} else {
    return 2;
}

if (a == 1) {
    return 1;
} else if (a == 2) {
    return 2;
} else {
    return 3;
}";
        todo!();
    }

    #[test]
    fn test_loop() {
        let src = "
let a: int = 0;
while (a < 10) {
    a = a + 1;
}";
        todo!();
    }
    
    #[test]
    fn test_asgmt() {
        let src = r#"
let a: int = 1; let b: float = 2.0; let c: bool = true; let d: char = 'a'; let e: string = "hello"; let f: [int] = [1, 2, 3, 4, 5];
"#;

        let stmts = parse_from(src);

        assert_eq!(
            stmts[0],
            (
                Stmt::Declaration(Decl::Let {
                    id: "a".to_string(),
                    type_: Type::Int,
                    value: Some(Expr::Constant(Literal::Int(1)))
                }),
                1..16
            ) 
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Declaration(Decl::Let {
                    id: "b".to_string(),
                    type_: Type::Float,
                    value: Some(Expr::Constant(Literal::Float(2.0)))
                }),
                17..36
            ) 
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Declaration(Decl::Let {
                    id: "c".to_string(),
                    type_: Type::Bool,
                    value: Some(Expr::Constant(Literal::Bool(true)))
                }),
                37..56
            ) 
        );

        assert_eq!(
            stmts[3],
            (
                Stmt::Declaration(Decl::Let {
                    id: "d".to_string(),
                    type_: Type::Char,
                    value: Some(Expr::Constant(Literal::Char('a')))
                }),
                57..75
            ) 
        );

        assert_eq!(
            stmts[4],
            (
                Stmt::Declaration(Decl::Let {
                    id: "e".to_string(),
                    type_: Type::String,
                    value: Some(Expr::Constant(Literal::String("hello".to_string())))
                }),
                76..100
            ) 
        );

        assert_eq!(
            stmts[5],
            (
                Stmt::Declaration(Decl::Let {
                    id: "f".to_string(),
                    type_: Type::Array(Box::new(Type::Int)),
                    value: Some(Expr::Array(vec![
                        (Expr::Constant(Literal::Int(1)), 117..118),
                        (Expr::Constant(Literal::Int(2)), 120..121),
                        (Expr::Constant(Literal::Int(3)), 123..124),
                        (Expr::Constant(Literal::Int(4)), 126..127),
                        (Expr::Constant(Literal::Int(5)), 129..130),
                    ]))
                }),
                101..132
            ) 
        );
    }

    #[test]
    fn test_arithmetic_exp() {
        let src = "
let a: int = 1 + 2;
let b: int = 1 + 2 + 3;
let c: int = 1 - a * b / 2 + 2;
let d: int = (1 - a) * b / (2 + 2) + 3;
";

        todo!();
    }

    #[test]
    fn test_boolean_exp() {
        let src = "
let a: bool = true;
let b: bool = false;
let c: bool = a && b;
let d: bool = a || b;
let e: bool = !a;
";
        todo!();
    }

    #[test]
    fn test_chain_exp() {
        let src = "
class Foo {
    fun foo() {
        return 1;
    }
}

let a: int = Foo.foo();
";

        todo!();
    }
}
