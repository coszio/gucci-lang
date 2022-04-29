use std::fmt::Display;

use chumsky::{
    prelude::{filter_map, just, skip_then_retry_until, recursive, Simple, nested_delimiters, choice},
    Parser, select
};

use crate::lexer::{Token, Span, Op};

type Spanned<T> = (T, Span);
type Block = Vec<Spanned<Stmt>>;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Decl(Decl),
    Assign {
        to: Field, 
        value: Spanned<Expr>,
    },
    Cond {
        if_: Spanned<Expr>,
        then: Block,
        elif: Vec<(Spanned<Expr>, Block)>,
        else_: Option<Block>,
    },
    Loop(Loop),
    Expr(Spanned<Expr>),
    Return(Spanned<Expr>),
    Error,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    name: String,
    child: Option<Box<Self>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Let { 
        name: String,
        type_: Type,
        value: Option<Spanned<Expr>>,
    },
    Fun(Fun),
    Class {
        name: String,
        inherits: Option<String>,
        implements: Option<String>,
        has: Vec<Spanned<Var>>,
        does: Vec<Spanned<Fun>>,
    },
    Interface {
        name: String,
        should_do: Vec<Spanned<FunSignature>>,
    },
}

#[derive(Debug, Clone, PartialEq)]
struct Var {
    name: String,
    type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Fun {
    name: String,
    params: Vec<Spanned<Var>>,
    ret_type: Option<Type>,
    body: Block,
}

#[derive(Debug, Clone, PartialEq)]
struct FunSignature {
    name: String,
    params: Vec<Spanned<Var>>,
    ret_type: Option<Type>,
}


#[derive(Debug, Clone, PartialEq)]
pub enum Loop {
    While {
        cond: Spanned<Expr>,
        body: Block,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Error,
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
    //// Structural
    Chain,

    //// Arithmetic
    Add,
    Sub,
    Mul,
    Div,

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
    String,

    //// Self referencing
    This,

    //// Custom
    Custom(String),
}


fn parser() -> impl Parser<Token, Vec<Spanned<Stmt>>, Error = Simple<Token>> {
    let ident = select! { Token::Ident(name) => name.clone() }.labelled("identifier");
    
    let this = just(Token::This).to("this".to_string());
    
    let single_field = ident.clone()
        .or(this.clone())
        .map(|name| Field { name, child: None });

    let field = single_field
        .clone()
        .then_ignore(just(Token::Op(Op::Dot)))
        .repeated()        
        .then(single_field.clone())
        .foldr(|a, b| {
            Field { 
                child: Some(Box::new(b)),
                ..a
            }
        }
        );
    
    let expr = recursive(|expr| {
        
        let ident = ident
            .clone()
            .or(this)
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
            .or(ident.clone())
            .or(array)
            .map_with_span(|expr, span| (expr, span))
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                // Attempt to recover anything that looks like a parenthesised expression but contains errors
                .recover_with(nested_delimiters(
                    Token::Ctrl('('),
                    Token::Ctrl(')'),
                    [
                        (Token::Ctrl('['), Token::Ctrl(']')),
                        (Token::Ctrl('{'), Token::Ctrl('}')),
                    ],
                    |span| (Expr::Error, span),
                ))
                .map_with_span(|(expr, _), span| (expr, span)));

        let args = items
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')')))
                .map_with_span(|arg, span: Span| (arg, span))
                .repeated();

        // function calls have highest precedence   
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
        
        // chained expression
        let op = just(Token::Op(Op::Dot))
            .to(BinOp::Chain);

        let chain = fun_call
            .clone()
            .then(op.then(fun_call).repeated())
            .foldl(|f, (op, g)| {
                let span = f.1.start..g.1.end;
                (Expr::Binary {
                    lhs: Box::new(f), 
                    op, 
                    rhs: Box::new(g)
                }, span)
            });

        let op = just(Token::Op(Op::Mul))
            .to(BinOp::Mul)
            .or(just(Token::Op(Op::Div))
                .to(BinOp::Div));
        let product = chain
            .clone()
            .then(op.then(chain).repeated())
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
                _ => Type::Custom(t)
            },
            Token::This => Type::This,
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
        .then(type_.clone())
        .map(|(name, type_)| Var { name, type_ });

    // A let expression (with optional initialization)
    let let_ = just(Token::Let)
        .ignore_then(var.clone())
        .then(just(Token::Op(Op::Assign))
            .ignore_then(expr.clone())
            .or_not())
        .map(|(var, value)| 
            Stmt::Decl(Decl::Let { name: var.name, type_: var.type_, value }));

    // An assignment
    let assign = field
        .clone()
        .then(just(Token::Op(Op::Assign))
            .ignore_then(expr.clone()))
        .map(|(field, value)| 
            Stmt::Assign { to: field, value });

    // A return statement
    let return_ = just(Token::Return)
        .ignore_then(expr.clone())
        .map(|value| Stmt::Return(value));

    // Parameters used in function declarations
    let params = var
        .clone()
        .map_with_span(|var, span| (var, span))
        .separated_by(just(Token::Ctrl(',')));

    // A block is a collection of statements
    let block = recursive(|block| {

        let block = block
            .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}')))
            // Attempt to recover anything that looks like a block but contains errors
            .recover_with(nested_delimiters(
                Token::Ctrl('{'),
                Token::Ctrl('}'),
                [
                    (Token::Ctrl('('), Token::Ctrl(')')),
                    (Token::Ctrl('['), Token::Ctrl(']')),
                ],
                |span| vec![(Stmt::Error, span)],
            ));    

        // A while Loop
        let while_ = just(Token::While)
            .ignore_then(expr.clone())
            .then(block.clone())
            .map(|(cond, body)| Stmt::Loop(Loop::While { cond, body }));

        // A function declaration
        let fun_signature = just(Token::Fun)
            .ignore_then(ident.clone())
            .then(params
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))))
            .then(just(Token::Ctrl(':')).ignore_then(type_.clone()).or_not())
            .map(|((name, params), ret_type)| 
                FunSignature { name, params, ret_type });

        let fun_decl = fun_signature
            .clone()
            .then(block.clone())
            .map(|(signature, body)| 
                Fun { 
                    name: signature.name, 
                    params: signature.params, 
                    ret_type: signature.ret_type, 
                    body 
                });

        let fun = fun_decl.clone()
            .map(|fun| Stmt::Decl(Decl::Fun(fun)));

        // A conditional statement
        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then(block.clone())
            .then(just(Token::Else)
                .ignore_then(just(Token::If))
                .ignore_then(expr.clone())
                .then(block.clone())
                .repeated())
            .then(just(Token::Else)
                .ignore_then(block.clone())
                .or_not())
            .map(|(((if_, then), elif), else_)|
                Stmt::Cond { if_, then, elif, else_ });

        
        // A class declaration
        let has = just(Token::Has)
                .ignore_then(just(Token::Ctrl(':')))
                .ignore_then(var.clone()
                    .then_ignore(just(Token::Ctrl(';')))         
                    .map_with_span(|var, span| (var, span))           
                    .repeated())
                .or_not()
                .map(|vars| vars.unwrap_or_else(|| Vec::new()));

        let does = just(Token::Does)
            .ignore_then(just(Token::Ctrl(':')))
            .ignore_then(fun_decl.clone()
                .map_with_span(|fun, span| (fun, span))
                .repeated())
            .or_not()
            .map(|funcs| funcs.unwrap_or_else(|| Vec::new()));

        let class = just(Token::Class)
            .ignore_then(ident.clone())
            .then(just(Token::Inherits).ignore_then(ident.clone()).or_not())
            .then(just(Token::Implements).ignore_then(ident.clone()).or_not())
            .then(has
                .then(does)
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))))
            .map(|(((name, inherits), implements), (has, does))| 
                Stmt::Decl(Decl::Class { name, inherits, implements, has, does }));

        let interface = just(Token::Interface)
            .ignore_then(ident.clone())
            .then(fun_signature
                .then_ignore(just(Token::Ctrl(';')))
                .map_with_span(|fun_sig, span| (fun_sig, span))
                .repeated()
                .delimited_by(just(Token::Ctrl('{')), just(Token::Ctrl('}'))))
            .map(|(name, should_do)| 
                Stmt::Decl(Decl::Interface { name, should_do }));


        // All possible statements
        let stmt = choice((
                let_,
              assign,
              return_,
            ))
            .then_ignore(just(Token::Ctrl(';')))
            .or(choice((
                fun,
                if_,
                while_,
                class,
                interface,
            )))
            .map_with_span(|stmt, span: Span| (stmt, span));
            
        stmt.repeated()

    });

    block

}

#[cfg(test)]
mod tests {

    use chumsky::Stream;

    use crate::lexer::lexer;

    use super::*;

    fn parse_from(input: &str) -> Vec<Spanned<Stmt>> {
        let (tokens, lex_errs) = lexer().parse_recovery(input);
        assert!(lex_errs.is_empty());
        let tokens = tokens.unwrap();

        println!("Tokens: {:?}", tokens);
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
fun baz(a: int, b: float) { }
";

        let stmts = parse_from(src);

        println!("stmts: {stmts:?}");

        assert_eq!(
            stmts[0],
            (
                Stmt::Decl(Decl::Fun(Fun {
                    name: "foo".to_string(),
                    params: vec![],
                    ret_type: Some(Type::Int), 
                    body: vec![
                        (Stmt::Return((Expr::Constant(Literal::Int(1)), 29..30)), 22..31)
                    ],
                })),
                1..33
            ) 
        );

        
        assert_eq!(
            stmts[1],
            (
                Stmt::Decl(Decl::Fun(Fun {
                    name: "bar".to_string(),
                    params: vec![
                        (Var {
                            name: "a".to_string(),
                            type_: Type::Int,
                        }, 42..48),
                        (Var {
                            name: "b".to_string(),
                            type_: Type::Float,
                        }, 50..58),
                    ],
                    ret_type: Some(Type::Float), 
                    body: vec![
                        (Stmt::Return((Expr::Binary {
                            lhs: Box::new((Expr::Ident( "a".to_string()), 80..81)),
                            op: BinOp::Add,
                            rhs: Box::new((Expr::Ident( "b".to_string()), 84..85))
                        }, 80..85)), 73..86),
                    ],
                })),
                34..88
            ) 
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Decl(Decl::Fun( Fun {
                    name: "baz".to_string(),
                    params: vec![
                        (Var {
                            name: "a".to_string(),
                            type_: Type::Int,
                        }, 97..103),
                        (Var {
                            name: "b".to_string(),
                            type_: Type::Float,
                        }, 105..113),
                    ],
                    ret_type: None, 
                    body: vec![],
                })),
                89..118
            ) 
        );
    }

    #[test]
    fn test_class_decl() {
        let src = "
class Foo {
    does:
        fun foo(): int {
            return 1;
        }
}

class Point inherits Foo {
    has:
        x: float;
        y: float;
    
    does:
        fun new(): this {
            this.a = 0.0;
            this.b = 0.0;
            return this;
        }
}
";

        
        let stmts = parse_from(src);

        println!("{:?}", stmts);

        assert_eq!(
            stmts[0],
            (
                Stmt::Decl(Decl::Class {
                    name: "Foo".to_string(),
                    inherits: None,
                    implements: None,
                    has: vec![],
                    does: vec![
                        (
                            Fun {
                                name: "foo".to_string(),
                                params: vec![],
                                ret_type: Some(Type::Int), 
                                body: vec![
                                    (Stmt::Return((Expr::Constant(Literal::Int(1)), 67..68)), 60..69)
                                ],
                            },
                            31..79
                        )
                    ],                    
                }),
                1..81
            )    
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Decl(Decl::Class {
                    name: "Point".to_string(),
                    inherits: Some("Foo".to_string()),
                    implements: None,
                    has: vec![
                        (Var {
                            name: "x".to_string(),
                            type_: Type::Float,
                        }, 127..136),
                        (Var {
                            name: "y".to_string(),
                            type_: Type::Float,
                        }, 145..154),
                    ],
                    does: vec![
                        (
                            Fun {
                                name: "new".to_string(),
                                params: vec![],
                                ret_type: Some(Type::This), 
                                body: vec![
                                    (Stmt::Assign {
                                        to: Field { 
                                            name: "this".to_string(),
                                            child: Some(Box::new(Field { 
                                                name: "a".to_string(),
                                                child: None
                                            })),
                                        },
                                        value: (Expr::Constant(Literal::Float(0.0)), 217..220),
                                    }, 208..221),
                                    (Stmt::Assign {
                                        to: Field { 
                                            name: "this".to_string(),
                                            child: Some(Box::new(Field { 
                                                name: "b".to_string(),
                                                child: None
                                            })),
                                        },
                                        value: (Expr::Constant(Literal::Float(0.0)), 243..246),
                                    }, 234..247),
                                    (Stmt::Return((Expr::Ident( "this".to_string()), 267..271)), 260..272),
                                ],
                            },
                            178..282
                        )
                    ],                    
                }),
                83..284
            )    
        );
    }

    #[test]
    fn test_interface_decl() {
        let src = "
interface Foo {
    fun foo();
}
interface Baz {
    fun faz(a: int, s: string);
}
";

        let stmts = parse_from(src);

        println!("{:?}", stmts);

        assert_eq!(
            stmts[0],
            (
                Stmt::Decl(Decl::Interface {
                    name: "Foo".to_string(),
                    should_do: vec![
                        (
                            FunSignature {
                                name: "foo".to_string(),
                                params: vec![],
                                ret_type: None, 
                            },
                            21..31
                        )
                    ],                    
                }),
                1..33
            )    
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Decl(Decl::Interface {
                    name: "Baz".to_string(),
                    should_do: vec![
                        (
                            FunSignature {
                                name: "faz".to_string(),
                                params: vec![
                                    (Var {
                                        name: "a".to_string(),
                                        type_: Type::Int,
                                    }, 62..68),
                                    (Var {
                                        name: "s".to_string(),
                                        type_: Type::String,
                                    }, 70..79),
                                ],
                                ret_type: None, 
                            },
                            54..81
                        )
                    ],                    
                }),
                34..83
            )    
        );
    }

    #[test]
    fn test_cond() {
        let src = "
if a == 1 {
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


        let stmts = parse_from(src);
        
        assert_eq!(
            stmts[0],
            (
                Stmt::Cond {
                    if_: (Expr::Binary {
                        lhs: Box::new((Expr::Ident( "a".to_string()), 4..5)),
                        op: BinOp::Eq,
                        rhs: Box::new((Expr::Constant(Literal::Int(1)), 9..10)),
                    }, 4..10),
                    then: vec![
                        (Stmt::Return((Expr::Constant(Literal::Int(1)), 24..25)), 17..26)
                    ],
                    elif: vec![],
                    else_: None,
                },
                1..28   
            )
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Cond {
                    if_: (Expr::Binary {
                        lhs: Box::new((Expr::Ident( "a".to_string()), 34..35)),
                        op: BinOp::Eq,
                        rhs: Box::new((Expr::Constant(Literal::Int(1)), 39..40)),
                    }, 33..41),
                    then: vec![
                        (Stmt::Return((Expr::Constant(Literal::Int(1)), 55..56)), 48..57)
                    ],
                    elif: vec![],
                    else_: Some(vec![
                        (Stmt::Return((Expr::Constant(Literal::Int(2)), 78..79)), 71..80)
                    ]),
                },
                30..82
            )
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Cond {
                    if_: (Expr::Binary {
                        lhs: Box::new((Expr::Ident( "a".to_string()), 88..89)),
                        op: BinOp::Eq,
                        rhs: Box::new((Expr::Constant(Literal::Int(1)), 93..94)),
                    }, 87..95),
                    then: vec![
                        (Stmt::Return((Expr::Constant(Literal::Int(1)), 109..110)), 102..111)
                    ],
                    elif: vec![
                        ((Expr::Binary {
                                lhs: Box::new((Expr::Ident( "a".to_string()), 123..124)),
                                op: BinOp::Eq,
                                rhs: Box::new((Expr::Constant(Literal::Int(2)), 128..129)),
                            }, 122..130), 
                            vec![
                                (Stmt::Return((Expr::Constant(Literal::Int(2)), 144..145)), 137..146)
                            ],                         
                        ),
                    ],
                    else_: Some(vec![
                        (Stmt::Return((Expr::Constant(Literal::Int(3)), 167..168)), 160..169),
                    ]),
                },
                84..171
            )
        );
    }

    #[test]
    fn test_while_loop() {
        let src = "
let a: int = 0;
while (a < 10) {
    a = a + 1;
}";

        let stmts = parse_from(src);

        assert_eq!(
            stmts[1],
            (
                Stmt::Loop(Loop::While {
                    cond: (Expr::Binary {
                        lhs: Box::new((Expr::Ident( "a".to_string()), 24..25)),
                        op: BinOp::Lt,
                        rhs: Box::new((Expr::Constant(Literal::Int(10)), 28..30)),
                    }, 23..31),
                    body: vec![
                        (Stmt::Assign {
                            to: Field { 
                                name: "a".to_string(),
                                child: None
                         },
                            value: (Expr::Binary {
                                lhs: Box::new((Expr::Ident( "a".to_string()), 42..43)),
                                op: BinOp::Add,
                                rhs: Box::new((Expr::Constant(Literal::Int(1)), 46..47)),
                            }, 42..47),
                        }, 38..48),
                    ],
                }),
                17..50
            )
        );
    
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
    fn test_field_asgmt() {
        
        let src = "
a.b = 5;
a.b.c.d = 7;
this.x = 0;
";

        let stmts = parse_from(src);

        assert_eq!(
            stmts[0],
            (
                Stmt::Assign {
                    to: Field { 
                        name: "a".to_string(), 
                        child: Some(Box::new(Field {
                            name: "b".to_string(), 
                            child: None
                        }))
                    },
                    value: (Expr::Constant(Literal::Int(5)), 7..8),
                },
                1..9
            )
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Assign {
                    to: Field {
                        name: "a".to_string(),
                        child: Some(Box::new(Field {
                            name: "b".to_string(),
                            child: Some(Box::new(Field {
                                name: "c".to_string(),
                                child: Some(Box::new(Field { name:"d".to_string(), child: None}))
                            }))
                        }))
                    },
                    value: (Expr::Constant(Literal::Int(7)), 20..21),
                },
                10..22
            )
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Assign {
                    to: Field { 
                        name:"this".to_string(), 
                        child: Some(Box::new(Field {
                            name: "x".to_string(),
                            child: None
                        }))
                    },
                    value: (Expr::Constant(Literal::Int(0)), 32..33),
                },
                23..34
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
                    to: Field { 
                        name: "a".to_string(),
                        child: None
                    },
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
                    to: Field { 
                        name: "b".to_string(),
                        child: None
                    },
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
                    to: Field { 
                        name: "c".to_string(),
                        child: None
                    },
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Binary {
                            lhs: Box::new((Expr::Constant(Literal::Int(1)), 31..32)), 
                            op: BinOp::Sub, 
                            rhs: Box::new((Expr::Binary {
                                    lhs: Box::new((Expr::Binary {
                                        lhs: Box::new((Expr::Ident( "a".to_string()), 35..36)), 
                                        op: BinOp::Mul, 
                                        rhs: Box::new((Expr::Ident( "b".to_string()), 39..40)), 
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
                    to: Field { 
                        name: "d".to_string(),
                        child: None
                    },
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Binary {
                            lhs: Box::new((Expr::Binary {
                                lhs: Box::new((Expr::Binary {
                                        lhs: Box::new((Expr::Constant(Literal::Int(1)), 55..56)), 
                                        op: BinOp::Sub, 
                                        rhs: Box::new((Expr::Ident( "a".to_string()), 59..60)), 
                                    }, 54..61)),
                                op: BinOp::Mul, 
                                rhs: Box::new((Expr::Ident( "b".to_string()), 64..65)), 
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
                    to: Field { 
                        name: "a".to_string(),
                        child: None
                    },
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
                    to: Field { 
                        name: "a".to_string(),
                        child: None
                    },
                    value: (Expr::Call {
                        fun: Box::new((Expr::Ident( "foo".to_string()), 5..8)), 
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
                    to: Field { 
                        name: "b".to_string(),
                        child: None
                    },
                    value: (Expr::Call {
                        fun: Box::new((Expr::Ident( "foo".to_string()), 16..19)), 
                        args: vec![
                            (Expr::Ident( "a".to_string()), 20..21),
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
                    to: Field { 
                        name: "c".to_string(),
                        child: None
                    },
                    value: (Expr::Call {
                        fun: Box::new((Expr::Ident( "foo".to_string()), 28..31)), 
                        args: vec![
                            (Expr::Binary {
                                lhs: Box::new((Expr::Ident( "a".to_string()), 32..33)), 
                                op: BinOp::Add, 
                                rhs: Box::new((Expr::Ident( "b".to_string()), 34..35)), 
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
                    to: Field { 
                        name: "c".to_string(),
                        child: None
                    },
                    value: (Expr::Call {
                        fun: Box::new((Expr::Ident( "foo".to_string()), 42..45)), 
                        args: vec![
                            (Expr::Ident( "a".to_string()), 46..47),
                            (Expr::Ident( "b".to_string()), 49..50),
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
                    to: Field { 
                        name: "a".to_string(),
                        child: None
                    },
                    value: (Expr::Constant(Literal::Bool(true)), 5..9),
                },
                1..10
            )
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Assign {
                    to: Field { 
                        name: "b".to_string(),
                        child: None
                    },
                    value: (Expr::Constant(Literal::Bool(false)), 15..20),
                },
                11..21
            )
        );

        assert_eq!(
            stmts[2],
            (
                Stmt::Assign {
                    to: Field { 
                        name: "c".to_string(),
                        child: None
                    },
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Ident( "a".to_string()), 26..27)), 
                        op: BinOp::And, 
                        rhs: Box::new((Expr::Ident( "b".to_string()), 31..32)), 
                    }, 26..32),
                },
                22..33
            )
        );

        assert_eq!(
            stmts[3],
            (
                Stmt::Assign {
                    to: Field { 
                        name: "d".to_string(),
                        child: None
                    },
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Ident( "a".to_string()), 38..39)), 
                        op: BinOp::Or, 
                        rhs: Box::new((Expr::Ident( "b".to_string()), 43..44)), 
                    }, 38..44),
                },
                34..45
            )
        );

        assert_eq!(
            stmts[4],
            (
                Stmt::Assign {
                    to: Field { 
                        name: "e".to_string(),
                        child: None
                    },
                    value: (Expr::Unary {
                        op: UnOp::Not,
                        rhs: Box::new((Expr::Ident( "a".to_string()), 51..52)),
                    }, 50..52),
                },
                46..53
            )
        );

        assert_eq!(
            stmts[5],
            (
                Stmt::Assign {
                    to: Field { 
                        name: "f".to_string(),
                        child: None
                    },
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
a = foo.a.b;
foo = Foo.new();
";

        let stmts = parse_from(src);

        println!("{:?}", stmts);

        assert_eq!(
            stmts[0],
            (
                Stmt::Assign {
                    to: Field { 
                        name: "a".to_string(),
                        child: None
                    },
                    value: (Expr::Binary {
                        lhs: Box::new((Expr::Binary {
                                lhs: Box::new((Expr::Ident( "foo".to_string()), 5..8)), 
                                op: BinOp::Chain, 
                                rhs: Box::new((Expr::Ident( "a".to_string()), 9..10)), 
                            }, 5..10)), 
                        op: BinOp::Chain, 
                        rhs: Box::new((Expr::Ident( "b".to_string()), 11..12)), 
                    }, 5..12),
                },
                1..13
            )
        );

        assert_eq!(
            stmts[1],
            (
                Stmt::Assign { 
                    to: Field { name: "foo".to_string(), child: None }, 
                    value: (Expr::Binary { 
                        lhs: Box::new((Expr::Ident("Foo".to_string()), 20..23)), 
                        op: BinOp::Chain, 
                        rhs: Box::new((Expr::Call { 
                            fun: Box::new((Expr::Ident("new".to_string()), 24..27)), 
                            args: vec![]
                        }, 24..29)) 
                    }, 20..29) 
                }, 14..30)
        );
    }
}
