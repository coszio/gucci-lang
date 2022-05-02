
use crate::{
  directory::{self, Error}, 
  parser::{Stmt, Decl, Expr, Loop, Type, Literal, BinOp}, 
  lexer::Span
};


type Result<T> = std::result::Result<T, Error>;


fn eval_expr(expr: Expr, span: Span) -> Result<Type> {
  match expr {
    Expr::Error => todo!(),
    Expr::Call { fun, args } => todo!(),
    Expr::Binary { lhs, op, rhs } => match op {
        crate::parser::BinOp::Chain => todo!(),
        crate::parser::BinOp::Add => todo!(),
        crate::parser::BinOp::Sub => todo!(),
        crate::parser::BinOp::Mul => todo!(),
        crate::parser::BinOp::Div => todo!(),
        crate::parser::BinOp::Eq => todo!(),
        crate::parser::BinOp::Ne => todo!(),
        crate::parser::BinOp::Lt => todo!(),
        crate::parser::BinOp::Gt => todo!(),
        crate::parser::BinOp::Lte => todo!(),
        crate::parser::BinOp::Gte => todo!(),
        crate::parser::BinOp::And => todo!(),
        crate::parser::BinOp::Or => todo!(),
    },
    Expr::Unary { op, rhs } => match op {
        crate::parser::UnOp::Not => todo!(),
        crate::parser::UnOp::Neg => todo!(),
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