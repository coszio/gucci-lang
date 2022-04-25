use std::fmt::Display;

use chumsky::{
    prelude::{filter_map, just, skip_then_retry_until, recursive, Simple, nested_delimiters},
    Parser, select
};

use crate::lexer::{Token, Span, lexer, Op};

type Spanned<T> = (T, Span);
type Block = Vec<Spanned<Stmt>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Decl(Decl),
    Assign {
        id: String, 
        value: Spanned<Expr>,
    },
    // Condition(Spanned<Cond>),
    Loop(Loop),
    Expr(Spanned<Expr>),
    Return(Spanned<Expr>),
    Error,
    // Class(ClassDecl),
    // Interface(InterfaceDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Let { 
        name: String,
        type_: Type,
        value: Option<Spanned<Expr>>,
    },
    Fun {
        name: String,
        params: Vec<Spanned<Param>>,
        ret_type: Option<Type>,
        body: Block,
    },
    // Class {
    //     name: String,
    //     parent: Option<String>,
    //     interface: Option<String>,
    //     body: Block,
    // },
    // Interface(InterfaceDecl),
}
// class Point {
//     + public_field1: int;
//     # protected_field: int;
//     - private_field: int;
//     static static_field: int;

//     fun init(): Point {
//         @.field1 = 0;
//         @.field2 = 0;
//         return @;
//     }
// }
// let a: Point = Point.new();
// 

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
        fun: Box<Spanned<Self>>, 
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
    Parenthesized(Box<Spanned<Self>>),
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
    Ne,
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
    Neg,
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
struct Param {
    pub name: String,
    pub type_: Type,
}

struct Cond {
    if_: Spanned<Expr>,
    then: Block,
    elif: Option<Box<Self>>,
    else_: Option<Block>,
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct Block {
//     pub stmts: Vec<Stmt>,
// }

// pub struct ClassDecl {
//     pub name: String,
//     pub parent: Option<String>,
//     pub interface: Option<String>,
//     pub vars: Vec<(Var, Visibility)>,
//     pub methods: Vec<(Method, Visibility)>,
// }

// pub struct InterfaceDecl {
//     pub name: String,
//     pub body: Block,
// }


fn parser() -> impl Parser<Token, Vec<Spanned<Stmt>>, Error = Simple<Token>> {
    let ident = select! { Token::Ident(name) => name.clone() }.labelled("identifier");
    
    let expr = recursive(|expr| {
        let ident = ident.clone()
            .map(Expr::Ident);
            
        let number = filter_map(|span: Span, tok| {
                match tok {
                    Token::Int(i) => Ok(Expr::Constant(Literal::Int(i.parse().unwrap()))),
                    Token::Float(f) => Ok(Expr::Constant(Literal::Float(f.parse().unwrap()))),
                    _ => Err(Simple::custom(span, "Not a number, expected int or float")),
                }
            })
            .labelled("number");

        let value = select! {
            Token::Bool(b) => Expr::Constant(Literal::Bool(b)),
            Token::Char(c) => Expr::Constant(Literal::Char(c)),
            Token::Str(s) => Expr::Constant(Literal::String(s)),
        }
        .or(number);
        
        let items = expr
            .clone()
            .separated_by(just(Token::Ctrl(',')))
            .allow_trailing();
            
        let array = items
            .clone()
            .delimited_by(just(Token::Ctrl('[')), just(Token::Ctrl(']')))
            .map(Expr::Array)
            .labelled("array");
            
        let atom = value
            .or(ident)
            .or(array)
            .map_with_span(|expr, span| (expr, span))
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                .map_with_span(|(expr, _), span| (expr, span)));

        let args = items
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                .map_with_span(|arg, span: Span| (arg, span))
                .repeated();

        let fun_call = atom
            .clone()
            .then(args)
            .foldl(|f, args| {
                let span = f.1.start..args.1.end;
                (Expr::Call {
                    fun: Box::new(f), 
                    args: args.0
                }, span)
            });            
        
        let op = just(Token::Op(Op::Mul))
            .to(BinOp::Mul)
            .or(just(Token::Op(Op::Div))
                .to(BinOp::Div));
        let product = fun_call
            .clone()
            .then(op.then(atom).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary {
                    lhs: Box::new(a), 
                    op, 
                    rhs: Box::new(b)
                }, span)
            });

        let op = just(Token::Op(Op::Not)).to(UnOp::Not)
            .or(just(Token::Op(Op::Sub)).to(UnOp::Neg))
            .map_with_span(|op, span| (op, span));
        let unary = op
            .then(product.clone())
            .map(|(op,a)| {
                let span = op.1.start..a.1.end;
                (Expr::Unary {
                    op: op.0, 
                    rhs: Box::new(a)
                }, span)})
            .or(product.clone());

        let op = just(Token::Op(Op::Add))
            .to(BinOp::Add)
            .or(just(Token::Op(Op::Sub))
                .to(BinOp::Sub));
        let sum = unary
            .clone()
            .then(op.then(unary).repeated())
            .foldl(|a, (op, b)| {
                    let span = a.1.start..b.1.end;                
                    (Expr::Binary {
                        lhs: Box::new(a), 
                        op, 
                        rhs: Box::new(b)
                    }, span)
                }
            );
            
        let op = filter_map( |span, tok| match tok {
                Token::Op(Op::Eq) => Ok(BinOp::Eq),
                Token::Op(Op::Ne) => Ok(BinOp::Ne),
                Token::Op(Op::Lt) => Ok(BinOp::Lt),
                Token::Op(Op::Gt) => Ok(BinOp::Gt),
                Token::Op(Op::Lte) => Ok(BinOp::Lte),
                Token::Op(Op::Gte) => Ok(BinOp::Gte),
                _ => Err(Simple::custom(span, "Not a comparison operator")),
            });
        let comparison = sum
            .clone()
            .then(op.then(sum).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary {
                    lhs: Box::new(a), 
                    op, 
                    rhs: Box::new(b)
                }, span)
            });
        
        let op = just(Token::Op(Op::And)).to(BinOp::And);
        let and_expr = comparison
            .clone()
            .then(op.then(comparison).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary {
                    lhs: Box::new(a), 
                    op, 
                    rhs: Box::new(b)
                }, span)
            });

        let op = just(Token::Op(Op::Or)).to(BinOp::Or);
        let or_expr = and_expr
            .clone()
            .then(op.then(and_expr).repeated())
            .foldl(|a, (op, b)| {
                let span = a.1.start..b.1.end;
                (Expr::Binary {
                    lhs: Box::new(a), 
                    op, 
                    rhs: Box::new(b)
                }, span)
            });

        or_expr

    });

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

        simple
            .or(array)
    });
    
    // A variable with type
    let var = ident
        .clone()
        .then_ignore(just(Token::Ctrl(':')))
        .then(type_.clone());

    // A let expression (with optional initialization)
    let let_ = just(Token::Let)
        .ignore_then(var.clone())
        .then(just(Token::Op(Op::Assign))
            .ignore_then(expr.clone())
            .or_not())
        .map(|((id, type_), value)| 
            Stmt::Decl(Decl::Let { name: id, type_, value }));

    // An assignment
    let assign = ident
        .then(just(Token::Op(Op::Assign))
            .ignore_then(expr.clone()))
        .map(|(id, value)| 
            Stmt::Assign { id, value });

    // A return statement
    let return_ = just(Token::Return)
        .ignore_then(expr.clone())
        .map(|value| Stmt::Return(value));

    // Parameters used in function declarations
    let params = var
        .clone()
        .map_with_span(|(name, type_), span| (Param {name, type_}, span))
        .separated_by(just(Token::Ctrl(',')));

    // A block is a collection of statements
    let block = recursive(|block| {

        // A function declaration
        let fun = just(Token::Fun)
            .ignore_then(ident)
            .then(params
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            .then(just(Token::Ctrl(':')).ignore_then(type_.clone()).or_not())
            .then(block
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))))
            // .recover_with(nested_delimiters(
            //     Token::Ctrl('{'), 
            //     Token::Ctrl('}'), 
            //     [], 
            //     |span| (Stmt::Error, span)))
            .map(|(((name, params), ret_type), body)| 
                Stmt::Decl(Decl::Fun { name, params, ret_type, body }));

        // All possible statements
        let stmt = let_
            .or(assign)
            .or(return_)
            .then_ignore(just(Token::Ctrl(';')))
            .or(fun)
            .map_with_span(|stmt, span: Span| (stmt, span));
            
        stmt.repeated()

    });

    block

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
        
        println!("parse errors: {:?}", parse_errs);

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
                Stmt::Decl(Decl::Let {
                    name: "a".to_string(),
                    type_: Type::Int,
                    value: None
                }),
                1..12
            ) 
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Decl(Decl::Let {
                    name: "b".to_string(),
                    type_: Type::Float,
                    value: None
                }),
                13..26
            ) 
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Decl(Decl::Let {
                    name: "c".to_string(),
                    type_: Type::Bool,
                    value: None
                }),
                27..39
            ) 
        );

        assert_eq!(
            stmts[3],
            (
                Stmt::Decl(Decl::Let {
                    name: "d".to_string(),
                    type_: Type::Char,
                    value: None
                }),
                40..52
            ) 
        );

        assert_eq!(
            stmts[4],
            (
                Stmt::Decl(Decl::Let {
                    name: "e".to_string(),
                    type_: Type::String,
                    value: None
                }),
                53..67
            ) 
        );

        assert_eq!(
            stmts[5],
            (
                Stmt::Decl(Decl::Let {
                    name: "f".to_string(),
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
fun foo(): int {
    return 1;
}
fun bar(a: int, b: float): float {
    return a + b;
}
fun baz(a: int, b: float) { }";

        let stmts = parse_from(src);

        assert_eq!(
            stmts[0],
            (
                Stmt::Decl(Decl::Fun {
                    name: "foo".to_string(),
                    params: vec![],
                    ret_type: Some(Type::Int), 
                    body: vec![
                        (Stmt::Return((Expr::Constant(Literal::Int(1)), 29..30)), 22..31)
                    ],
                }),
                1..33
            ) 
        );

        
        assert_eq!(
            stmts[1],
            (
                Stmt::Decl(Decl::Fun {
                    name: "bar".to_string(),
                    params: vec![
                        (Param {
                            name: "a".to_string(),
                            type_: Type::Int,
                        }, 42..48),
                        (Param {
                            name: "b".to_string(),
                            type_: Type::Float,
                        }, 50..58),
                    ],
                    ret_type: Some(Type::Float), 
                    body: vec![
                        (Stmt::Return((Expr::Binary {
                            lhs: Box::new((Expr::Ident("a".to_string()), 80..81)),
                            op: BinOp::Add,
                            rhs: Box::new((Expr::Ident("b".to_string()), 84..85))
                        }, 80..85)), 73..86),
                    ],
                }),
                34..88
            ) 
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Decl(Decl::Fun {
                    name: "baz".to_string(),
                    params: vec![
                        (Param {
                            name: "a".to_string(),
                            type_: Type::Int,
                        }, 97..103),
                        (Param {
                            name: "b".to_string(),
                            type_: Type::Float,
                        }, 105..113),
                    ],
                    ret_type: None, 
                    body: vec![],
                }),
                89..118
            ) 
        );
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
    fn interface_decl() {
        let src = "
interface Foo {
    fun foo();
}
interface Baz {
    fun faz(a: int, s: string);
}
";

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
let a: int = 1;
let b: float = 2.0;
let c: bool = true;
let d: char = 'a';
let e: string = "hello";
let f: [int] = [1, 2, 3, 4, 5];
"#;

        let stmts = parse_from(src);

        assert_eq!(
            stmts[0],
            (
                Stmt::Decl(Decl::Let {
                    name: "a".to_string(),
                    type_: Type::Int,
                    value: Some((Expr::Constant(Literal::Int(1)), 14..15))
                }),
                1..16
            ) 
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Decl(Decl::Let {
                    name: "b".to_string(),
                    type_: Type::Float,
                    value: Some((Expr::Constant(Literal::Float(2.0)), 32..35))
                }),
                17..36
            ) 
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Decl(Decl::Let {
                    name: "c".to_string(),
                    type_: Type::Bool,
                    value: Some((Expr::Constant(Literal::Bool(true)), 51..55))
                }),
                37..56
            ) 
        );

        assert_eq!(
            stmts[3],
            (
                Stmt::Decl(Decl::Let {
                    name: "d".to_string(),
                    type_: Type::Char,
                    value: Some((Expr::Constant(Literal::Char('a')), 71..74))
                }),
                57..75
            ) 
        );

        assert_eq!(
            stmts[4],
            (
                Stmt::Decl(Decl::Let {
                    name: "e".to_string(),
                    type_: Type::String,
                    value: Some((Expr::Constant(Literal::String("hello".to_string())), 92..99))
                }),
                76..100
            ) 
        );

        assert_eq!(
            stmts[5],
            (
                Stmt::Decl(Decl::Let {
                    name: "f".to_string(),
                    type_: Type::Array(Box::new(Type::Int)),
                    value: Some((Expr::Array(vec![
                        (Expr::Constant(Literal::Int(1)), 117..118),
                        (Expr::Constant(Literal::Int(2)), 120..121),
                        (Expr::Constant(Literal::Int(3)), 123..124),
                        (Expr::Constant(Literal::Int(4)), 126..127),
                        (Expr::Constant(Literal::Int(5)), 129..130),
                    ]), 116..131))
                }),
                101..132
            ) 
        );
    }

    #[test]
    fn test_arithmetic_exp() {
        let src = "
a = 1 + 2;
b = 1 + 2 + 3;
c = 1 - a * b / 2 + 2;
d = (1 - a) * b / (2 + 2) + 3;
a = -1 + 2;
";

        let stmts = parse_from(src);
        println!("{:?}", stmts);
        // (1 + 2)
        assert_eq!(
            stmts[0],
            (
                Stmt::Assign {
                    id: "a".to_string(),
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Constant(Literal::Int(1)), 5..6)), 
                        op: BinOp::Add, 
                        rhs: Box::new((Expr::Constant(Literal::Int(2)), 9..10))
                    }, 5..10)
                },
                1..11
            ) 
        );

        // ((1 + 2) + 3)
        assert_eq!(
            stmts[1],
            (
                Stmt::Assign {
                    id: "b".to_string(),
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Binary {
                            lhs: Box::new((Expr::Constant(Literal::Int(1)), 16..17)), 
                            op: BinOp::Add, 
                            rhs: Box::new((Expr::Constant(Literal::Int(2)), 20..21)), 
                        }, 16..21)),
                        op: BinOp::Add, 
                        rhs: Box::new((Expr::Constant(Literal::Int(3)), 24..25))
                    }, 16..25),
                },
                12..26
            ) 
        );

        // ((1 - ((a * b) / 2)) + 2)
        assert_eq!(
            stmts[2],
            (
                Stmt::Assign {
                    id: "c".to_string(),
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Binary {
                            lhs: Box::new((Expr::Constant(Literal::Int(1)), 31..32)), 
                            op: BinOp::Sub, 
                            rhs: Box::new((Expr::Binary {
                                    lhs: Box::new((Expr::Binary {
                                        lhs: Box::new((Expr::Ident("a".to_string()), 35..36)), 
                                        op: BinOp::Mul, 
                                        rhs: Box::new((Expr::Ident("b".to_string()), 39..40)), 
                                    }, 35..40)),
                                    op: BinOp::Div,
                                    rhs: Box::new((Expr::Constant(Literal::Int(2)), 43..44)), 
                                }, 35..44)),
                        }, 31..44)),
                        op: BinOp::Add,
                        rhs: Box::new((Expr::Constant(Literal::Int(2)), 47..48)),
                    },31..48),
                }, 
                27..49
            )
        );

        //  ((((1 - a) * b) / (2 + 2)) + 3)
        assert_eq!(
            stmts[3],
            (
                Stmt::Assign {
                    id: "d".to_string(),
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Binary {
                            lhs: Box::new((Expr::Binary {
                                lhs: Box::new((Expr::Binary {
                                        lhs: Box::new((Expr::Constant(Literal::Int(1)), 55..56)), 
                                        op: BinOp::Sub, 
                                        rhs: Box::new((Expr::Ident("a".to_string()), 59..60)), 
                                    }, 54..61)),
                                op: BinOp::Mul, 
                                rhs: Box::new((Expr::Ident("b".to_string()), 64..65)), 
                            }, 54..65)),
                            op: BinOp::Div,
                            rhs: Box::new((Expr::Binary {
                                lhs: Box::new((Expr::Constant(Literal::Int(2)), 69..70)), 
                                op: BinOp::Add, 
                                rhs: Box::new((Expr::Constant(Literal::Int(2)), 73..74)), 
                            }, 68..75)),
                        }, 54..75)),
                        op: BinOp::Add,
                        rhs: Box::new((Expr::Constant(Literal::Int(3)), 78..79)),
                    }, 54..79),
                },
                50..80
            )
        );

        // ((-1) + 2)
        assert_eq!(
            stmts[4],
            (
                Stmt::Assign {
                    id: "a".to_string(),
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Unary {
                            op: UnOp::Neg,
                            rhs: Box::new((Expr::Constant(Literal::Int(1)), 86..87)),
                        }, 85..87)), 
                        op: BinOp::Add, 
                        rhs: Box::new((Expr::Constant(Literal::Int(2)), 90..91))
                    }, 85..91)
                },
                81..92
            ) 
        );
    }

    #[test]
    fn test_function_calls() {
        let src = "
a = foo();
b = foo(a);
c = foo(a+b);
c = foo(a, b);
";

        let stmts = parse_from(src);
        println!("{:?}", stmts);

        assert_eq!(
            stmts[0],
            (
                Stmt::Assign {
                    id: "a".to_string(),
                    value: (Expr::Call {
                        fun: Box::new((Expr::Ident("foo".to_string()), 5..8)), 
                        args: vec![],
                    }, 5..10)
                }, 
                1..11
            ) 
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Assign {
                    id: "b".to_string(),
                    value: (Expr::Call {
                        fun: Box::new((Expr::Ident("foo".to_string()), 16..19)), 
                        args: vec![
                            (Expr::Ident("a".to_string()), 20..21),
                        ],
                    }, 16..22)
                }, 
                12..23
            ) 
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Assign {
                    id: "c".to_string(),
                    value: (Expr::Call {
                        fun: Box::new((Expr::Ident("foo".to_string()), 28..31)), 
                        args: vec![
                            (Expr::Binary {
                                lhs: Box::new((Expr::Ident("a".to_string()), 32..33)), 
                                op: BinOp::Add, 
                                rhs: Box::new((Expr::Ident("b".to_string()), 34..35)), 
                            }, 32..35),
                        ],
                    }, 28..36)
                }, 
                24..37
            ) 
        );

        assert_eq!(
            stmts[3],
            (
                Stmt::Assign {
                    id: "c".to_string(),
                    value: (Expr::Call {
                        fun: Box::new((Expr::Ident("foo".to_string()), 42..45)), 
                        args: vec![
                            (Expr::Ident("a".to_string()), 46..47),
                            (Expr::Ident("b".to_string()), 49..50),
                        ],
                    }, 42..51)
                }, 
                38..52
            ) 
        );

    }
    
    #[test]
    fn test_boolean_exp() {
        let src = "
a = true; b = false; c = a && b; d = a || b; e = !a; f = true || !false && a;
";

        let stmts = parse_from(src);
        println!("{:?}", stmts);

        assert_eq!(
            stmts[0],
            (
                Stmt::Assign {
                    id: "a".to_string(),
                    value: (Expr::Constant(Literal::Bool(true)), 5..9),
                },
                1..10
            )
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Assign {
                    id: "b".to_string(),
                    value: (Expr::Constant(Literal::Bool(false)), 15..20),
                },
                11..21
            )
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Assign {
                    id: "c".to_string(),
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Ident("a".to_string()), 26..27)), 
                        op: BinOp::And, 
                        rhs: Box::new((Expr::Ident("b".to_string()), 31..32)), 
                    }, 26..32),
                },
                22..33
            )
        );

        assert_eq!(
            stmts[3],
            (
                Stmt::Assign {
                    id: "d".to_string(),
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Ident("a".to_string()), 38..39)), 
                        op: BinOp::Or, 
                        rhs: Box::new((Expr::Ident("b".to_string()), 43..44)), 
                    }, 38..44),
                },
                34..45
            )
        );

        assert_eq!(
            stmts[4],
            (
                Stmt::Assign {
                    id: "e".to_string(),
                    value: (Expr::Unary {
                        op: UnOp::Not,
                        rhs: Box::new((Expr::Ident("a".to_string()), 51..52)),
                    }, 50..52),
                },
                46..53
            )
        );

        assert_eq!(
            stmts[5],
            (
                Stmt::Assign {
                    id: "f".to_string(),
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Constant(Literal::Bool(true)), 58..62)), 
                        op: BinOp::Or, 
                        rhs: Box::new((Expr::Binary {
                            lhs: Box::new((Expr::Unary {
                                op: UnOp::Not,
                                rhs: Box::new((Expr::Constant(Literal::Bool(false)), 67..72)),
                            }, 66..72)), 
                            op: BinOp::And, 
                            rhs: Box::new((Expr::Ident("a".to_string()), 76..77)), 
                        }, 66..77)), 
                    }, 58..77),
                },
                54..78
            )
        );
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
