use std::{fmt::Display, str::FromStr};

use crate::shared::{Span, Spanned};

pub(crate) type Block = Vec<Spanned<Stmt>>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Stmt {
    Decl(Decl),
    // Assign {
    //     to: Field, 
    //     value: Spanned<Expr>,
    // },
    Cond {
        if_: Spanned<Expr>,
        then: Block,
        elif: Vec<(Spanned<Expr>, Block)>,
        else_: Option<Block>,
    },
    Loop(Loop),
    Expr(Spanned<Expr>),
    Return(Spanned<Expr>),
    Print(Spanned<Expr>),
    Error,
}

// #[derive(Debug, Clone, PartialEq)]
// pub(crate) struct Field {
//     pub(crate) name: String,
//     pub(crate) child: Option<Box<Self>>,
// }
// impl ToString for Field {
//     fn to_string(&self) -> String {
//         let mut s = String::new();
//         s.push_str(&self.name);
//         if let Some(ref child) = self.child {
//             s.push_str(".");
//             s.push_str(&child.to_string());
//         }
//         s
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Decl {
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
pub(crate) struct Var {
    pub name: String,
    pub type_: Type,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Fun {
    pub name: String,
    pub params: Vec<Spanned<Var>>,
    pub ret_type: Option<Type>,
    pub body: Block,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunSignature {
    pub name: String,
    pub params: Vec<Spanned<Var>>,
    pub ret_type: Option<Type>,
}


#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Loop {
    While {
        cond: Spanned<Expr>,
        body: Block,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
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
pub(crate) enum BinOp {
    //// Assignment
    Assign,
    
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
impl Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::Chain => write!(f, "."),
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
            BinOp::Eq => write!(f, "=="),
            BinOp::Ne => write!(f, "!="),
            BinOp::Lt => write!(f, "<"),
            BinOp::Gt => write!(f, ">"),
            BinOp::Lte => write!(f, "<="),
            BinOp::Gte => write!(f, ">="),
            BinOp::And => write!(f, "&&"),
            BinOp::Or => write!(f, "||"),
            BinOp::Assign => write!(f, "="),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UnOp {
    Not,
    Neg,
}
impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOp::Not => write!(f, "-"),
            UnOp::Neg => write!(f, "!"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Literal {
    Int(i32),
    Float(f32),
    Bool(bool),
    Char(char),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Int(i) => write!(f, "i:{}", i),
            Literal::Float(float) => write!(f, "f:{}", float),
            Literal::Bool(b) => write!(f, "b:{}", b),
            Literal::Char(c) => write!(f, "c:{}", c),
            Literal::String(s) => write!(f, "s:{}", s),
        }
    }
}

impl Into<Type> for Literal {
    fn into(self) -> Type {
        match self {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::Bool(_) => Type::Bool,
            Literal::Char(_) => Type::Char,
            Literal::String(_) => Type::String,
        }
    }
}

impl FromStr for Literal {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let type_= &s[0..1];
        let value = &s[2..];

        let literal = match type_ {
            "i" => Literal::Int(value.parse::<i32>().unwrap()),
            "f" => Literal::Float(value.parse::<f32>().unwrap()),
            "c" => Literal::Char(value.parse::<char>().unwrap()),
            "b" => Literal::Bool(value.parse::<bool>().unwrap()),
            "s" => Literal::String(value.to_string()),
            _ => return Err(format!("Unknown literal type: {}", type_))
        };

        Ok(literal)
    }
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

    //// Custom
    Custom(String),

    //// Error handling
    Error,
}
impl Type {
    pub fn size(&self) -> usize {
        match self {
            // stored in the stack
            Type::Int => 32,
            Type::Float => 32,
            Type::Bool => 1,
            Type::Char => 8,

            // stored in the heap (unknown size)
            Type::Array(t) => 0,
            Type::String => 0,
            Type::Custom(s) => 0,
            Type::Error => 0,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Char => write!(f, "char"),
            Type::Array(t) => write!(f, "[{:?}]", t),
            Type::String => write!(f, "string"),
            Type::Custom(name) => write!(f, "{}", name),
            Type::Error => todo!(),
        }
    }
}

impl FromStr for Type {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "int" => Ok(Type::Int),
            "float" => Ok(Type::Float),
            "bool" => Ok(Type::Bool),
            "char" => Ok(Type::Char),
            _ => Err(format!("Unhandled type: {}", s))
        }
    }
}