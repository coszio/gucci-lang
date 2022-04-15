use std::fmt::Display;

use chumsky::{
    prelude::{filter, just, one_of, skip_then_retry_until, take_until, Simple},
    text::{self, TextParser},
    Parser,
};

use crate::lexer::{Token, Span};

// #[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Declaration(Decl),
    Assignment(Asgmt),
    Condition(Cond),
    Loop(Loop),
    Expression(Expr),
}

pub enum Decl {
    Var(String),
    Func(String, Vec<String>, Vec<Stmt>),
}

pub enum Asgmt {
    Var(String, Expr),
    Func(String, Vec<Expr>),
}

pub enum Cond {
    If(Expr, Vec<Stmt>),
    IfElse(Expr, Vec<Stmt>, Vec<Stmt>),
}

pub enum Loop {
    While(Expr, Vec<Stmt>),
}

pub enum Expr {
    AndExp(AndExp),
    OrExp(AndExp, Box<Self>),
}

pub enum AndExp {
    CmpExp(CmpExp),
    AndExp(CmpExp, Box<Self>),
}

pub enum CmpExp {
    TermExp(TermExp),
    CmpExp(TermExp, CmpOp, TermExp),
}

pub enum CmpOp {
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
}

pub enum TermExp {
    TermExp(Term, TermOp, Box<Self>),
    UnaryExp(UnaryOp, Box<Self>),
    Term(Term),
}

pub enum TermOp {
    Add,
    Sub,
}

pub enum UnaryOp {
    Not,
    Minus,
}

pub enum Term {
    Term(Factor, FacOp, Box<Self>),
    Factor(Factor),
}

pub enum FacOp {
    Mul,
    Div,
}

pub enum Factor {
    Expr(Box<Expr>),
    Const(Const),
    Ident(Ident),
    FunCall(FunCall),
    ChainExp(ChainExp),
}
pub struct FunCall {
    pub fun: Ident,
    pub args: Vec<Expr>,
}

pub enum ChainExp {
    Chain(Chain),
    ChainExp(Chain, Box<ChainExp>),
}

pub enum Chain {
    Ident(Ident),
    FunCall(FunCall)
}

pub enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
}

pub struct VarDecl {
    pub name: Ident,
    pub type_: Type,
    pub value: Option<Expr>,
}

pub enum Type {
    Primitive(PrimitiveType),
    Compound(CompoundType),
    Custom(CustomType),
}

pub enum PrimitiveType {
    Int,
    Float,
    Bool,
    Char,
}

pub enum CompoundType {
    Array(Box<Type>),
    Tuple(Vec<Box<Type>>),
    String,
}

pub enum CustomType {
    Ident(Ident),
}

pub struct FunDecl {
    pub name: Ident,
    pub args: Vec<ArgDecl>,
    pub ret_type: Type,
    pub body: Block,
}

pub struct ArgDecl {
    pub name: Ident,
    pub type_: Type,
}

pub struct Block {
    pub stmts: Vec<Stmt>,
    pub ret_type: Option<Type>,
}

pub struct ClassDecl {
    pub name: Ident,
    pub parent: Option<Ident>,
    pub interface: Option<Ident>,
    pub body: Block,
}

pub struct InterfaceDecl {
    pub name: Ident,
    pub body: Block,
}

pub struct Ident {
    pub name: String,
    pub kind: IdentKind, // Maybe not necessary, since we can infer it from the the struct in which it is defined
}

pub enum IdentKind {
    Var,
    Fun,
    Class,
    Interface,
}

// fn parser() -> impl Parser<(Token, Span), Vec<(Stmt)>, Error = Simple<char>> {


// }

#[cfg(test)]
mod tests {

    #[test]
    fn test_var_decl() {
        todo!();
    }

    #[test]
    fn test_fun_decl() {
        todo!();
    }

    #[test]
    fn test_class_decl() {
        todo!();
    }

    #[test]
    fn test_block() {
        todo!();
    }

    #[test]
    fn test_cond() {
        todo!();
    }

    #[test]
    fn test_loop() {
        todo!();
    }
    
    #[test]
    fn test_asgmt() {
        todo!();
    }

    #[test]
    fn test_arithmetic_exp() {
        todo!();
    }

    #[test]
    fn test_boolean_exp() {
        todo!();
    }

    #[test]
    fn test_chain_exp() {
        todo!();
    }
}