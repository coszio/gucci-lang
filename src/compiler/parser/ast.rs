use std::{fmt::Display, str::FromStr};

use crate::shared::{Span, Spanned};

pub(crate) type Block = Vec<Spanned<Stmt>>;

/// Building blocks of the AST, all the possible variants of a statement
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Stmt {
    Decl(Decl),
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

/// All the possible variants of a declaration
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

/// A variable with name and type
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Var {
    pub name: String,
    pub type_: Type,
}

/// A function declaration
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Fun {
    pub name: String,
    pub params: Vec<Spanned<Var>>,
    pub ret_type: Option<Type>,
    pub body: Block,
}

/// A function signature, used for implementing interfaces
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct FunSignature {
    pub name: String,
    pub params: Vec<Spanned<Var>>,
    pub ret_type: Option<Type>,
}

/// An statement that loops over a block of statements
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Loop {
    While {
        cond: Spanned<Expr>,
        body: Block,
    },
}

/// All the possible variants of an expression
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
}

/// Operators that act on two values
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

/// Operators that act on only one value
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum UnOp {
    Not,
    Neg,
}
impl Display for UnOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnOp::Not => write!(f, "!"),
            UnOp::Neg => write!(f, "-"),
        }
    }
}

/// A literal value like `1, 2.5, 'c'`, etc.
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