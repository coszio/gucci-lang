
use crate::{
  directory::{self, Error}, 
  parser::{Stmt, Decl, Expr, Loop, Type, Literal, BinOp, UnOp}, 
  lexer::Span,
  semantic_cube::resolve,
};

// static mut GLOBAL_DIR: directory::Scope = directory::Scope::new();

type Result<T> = std::result::Result<T, Error>;

fn eval_expr(expr: Expr, span: Span) -> Result<Type> {
  match expr {
    Expr::Error => todo!(),
    Expr::Call { fun, args } => todo!(),
    Expr::Binary { lhs, op, rhs } => todo!(),
    Expr::Unary { op, rhs } => match op {
        UnOp::Not => todo!(),
        UnOp::Neg => todo!(),
    },
    Expr::Constant(literal) => match literal {
        Literal::Int(_) => todo!(),
        Literal::Float(_) => todo!(),
        Literal::Bool(_) => todo!(),
        Literal::Char(_) => todo!(),
        Literal::String(_) => todo!(),
    },
    Expr::Ident(_) => todo!(),
    Expr::Array(_) => todo!(),
    Expr::Parenthesized(e) => eval_expr(e.0, e.1),
  }

}


fn eval_stmt(stmt: Stmt, span: Span) -> Result<()> {
  match stmt {
    Stmt::Decl(decl) => match decl {
        Decl::Let { name, type_, value } => todo!(),
        Decl::Fun(fun) => todo!(),
        Decl::Class { name, inherits, implements, has, does } => todo!(),
        Decl::Interface { name, should_do } => todo!(),
    }
    Stmt::Assign { to, value } => todo!(),
    Stmt::Cond { if_, then, elif, else_ } => todo!(),
    Stmt::Loop(loop_) => match loop_ {
        Loop::While { cond, body } => todo!(),
    }
    Stmt::Expr(_) => todo!(),
    Stmt::Return(_) => todo!(),
    Stmt::Error => todo!(),
}
}
// pub(crate) fn semantic_analysis(stmts: Vec<Stmt>) -> Result<()> {

//   let errors: Vec<Error> = stmts.iter()
//     .map(|stmt| {
//       match stmt {
//         Stmt::Decl(decl) => match decl {
//             Decl::Let { name, type_, value } => todo!(),
//             Decl::Fun(_) => todo!(),
//             Decl::Class { name, inherits, implements, has, does } => todo!(),
//             Decl::Interface { name, should_do } => todo!(),
//         },
//         Stmt::Assign { to, value } => todo!(),
//         Stmt::Cond { if_, then, elif, else_ } => todo!(),
//         Stmt::Loop(loop_) => match loop_ {
//             Loop::While { cond, body } => todo!(),
//         },
//         Stmt::Expr((e, s)) => eval_expr(*e, *s),
//         Stmt::Return(_) => todo!(),
//         Stmt::Error => todo!(),
//     }
//     })
//     .collect();

//   Ok(())
// }

#[cfg(test)]
mod tests {

  use super::*;

}