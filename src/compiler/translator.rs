use std::{fmt::Display, io::{BufWriter, Write}, sync::Mutex};

use lazy_static::lazy_static;

use crate::directory::{Dir, self, Key};

use super::{parser::ast::{Decl, Expr, Literal, Type, Stmt, Block}, semantics::{item::{Item, Kind}, Scope}};

#[derive(Debug, Clone)]
struct Constant {
  id: String,
  value: Literal,
}
impl Constant {
  fn new(id: String, value: Literal) -> Self {
    Constant {
      id,
      value,
    }
  }
}
impl Key for Constant {
    fn key(&self) -> &str {
        &self.id
    }
}

lazy_static! {
  static ref COUNTER: Mutex<usize> = Mutex::new(0);
}

fn new_id() -> String {
  let mut counter = COUNTER.lock().unwrap();
  let id = format!("{}", counter);
  *counter += 1;
  id
}

struct Quadruple {
    op: String,
    arg1: String,
    arg2: String,
    res: String,
}

impl Display for Quadruple {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "{},\t{},\t{},\t{}\n", self.op, self.arg1, self.arg2, self.res)
  }
}

// fn map_dir_errs(err: directory::Error) -> Error {
//     match err {
//         directory::Error::Nonexistent(key) => Error::Nonexistent(key),
//         directory::Error::Duplicate(key) => Error::Duplicate(key),
//     }
//   }

fn translate_expr(buffer: &mut BufWriter<Vec<u8>>, expr: Expr, type_dir: &mut Scope, const_dir: &mut Dir<Constant>) -> String {
  match expr {
    Expr::Binary { lhs, op, rhs } => {
      let lhs_res = translate_expr(buffer, lhs.clone().0, type_dir, const_dir);
      let rhs_res = translate_expr(buffer, rhs.clone().0, type_dir, const_dir);
  
      // let res_type = resolve(
      //   &eval_expr(lhs.0, lhs.1, type_dir).unwrap(),
      //    &op, 
      //    &eval_expr(rhs.0, rhs.1, type_dir).unwrap())
      //    .unwrap();

      let dest = format!("t{}", new_id());
      
      let quad = Quadruple {
        op: op.to_string(),
        arg1: lhs_res,
        arg2: rhs_res,
        res: dest.clone(),
      };

      write!(buffer, "{}", quad).unwrap();

      dest
    }
    Expr::Ident(name) => name,
    Expr::Constant(literal) => {
      let unique_id = new_id();
      let item = match literal {
        Literal::Int(_) => Item::new(format!("i{}", unique_id), Kind::Literal, Type::Int),
        Literal::Float(_) => Item::new(format!("f{}", unique_id), Kind::Literal, Type::Float),
        Literal::Bool(_) => Item::new(format!("b{}", unique_id), Kind::Literal, Type::Bool),
        Literal::Char(_) => Item::new(format!("c{}", unique_id), Kind::Literal, Type::Char),
        Literal::String(_) => Item::new(format!("s{}", unique_id), Kind::Literal, Type::String),
      };

      type_dir.create(item.clone()).unwrap();

      // save it to the constants directory
      const_dir.create(Constant::new(item.id.clone(), literal)).unwrap();
      
      item.id
    },

    Expr::Error => todo!(),
    Expr::Call { fun, args } => todo!(),
    Expr::Unary { op, rhs } => {
      let rhs_res = translate_expr(buffer, rhs.clone().0, type_dir, const_dir);

      let dest = format!("t{}", new_id());

      let quad = Quadruple {
        op: op.to_string(),
        arg1: rhs_res,
        arg2: "".to_string(),
        res: dest.clone(),
      };

      write!(buffer, "{}", quad).unwrap();

      dest
    },
    Expr::Array(_) => todo!(),
    Expr::Parenthesized(_) => todo!(),
    _ => todo!(),
  }
}

fn translate_stmt(buffer: &mut BufWriter<Vec<u8>>, stmt: Stmt, type_dir: &mut Scope, const_dir: &mut Dir<Constant>) -> String {
  
  match stmt {
    Stmt::Decl(decl) => match decl {
        Decl::Let { name, type_, value } => {
          if let Some(value) = value {

            let value_id = translate_expr(buffer, value.0, type_dir, const_dir);
            let item = Item::new(name, Kind::Var, type_);

            type_dir.create(item.clone()).unwrap();

            let quad = Quadruple {
              op: "=".to_string(),
              arg1: value_id,
              arg2: "".to_string(),
              res: item.id.clone(),
            };

            write!(buffer, "{}", quad).unwrap();

            return item.id
          }
          else {
            todo!();
          }
        },
        Decl::Fun(_) => todo!(),
        Decl::Class { name, inherits, implements, has, does } => todo!(),
        Decl::Interface { name, should_do } => todo!(),
    },
    Stmt::Assign { to, value } => {
      let value_id = translate_expr(buffer, value.0, type_dir, const_dir);

      let quad = Quadruple {
        op: "=".to_string(),
        arg1: value_id,
        arg2: "".to_string(),
        res: to.to_string(),
      };

      write!(buffer, "{}\n", quad).unwrap();

      to.to_string()
    },
    Stmt::Cond { if_, then, elif, else_ } => todo!(),
    Stmt::Loop(_) => todo!(),
    Stmt::Expr((expr, span)) => translate_expr(buffer, expr, type_dir, const_dir),
    Stmt::Return(_) => todo!(),
    Stmt::Error => todo!(),
}
}

pub(crate) fn translate(stmts: Block) -> Result<String, ()> {

  let mut scope = Scope::new();
  
  let mut buffer = BufWriter::new(Vec::new());

  let mut const_dir: Dir<Constant> = directory::Dir::new();
  for (stmt, span) in stmts {
    let stmt_id = translate_stmt(&mut buffer, stmt, &mut scope, &mut const_dir);
  }
    
  println!("{:?}", const_dir);

  Ok(String::from_utf8(buffer.into_inner().unwrap()).unwrap())

}

mod tests {

  use crate::compiler::parser::ast::BinOp;

  use super::*;

  #[test]
  fn test_translate_expr() {
    let mut buffer = BufWriter::new(Vec::new());
    let mut scope = Scope::new();
    let mut const_dir = directory::Dir::new();

    let stmt = Stmt::Expr((Expr::Binary {
      lhs: Box::new((Expr::Constant(Literal::Int(1)), 0..0)),
      op: BinOp::Add,
      rhs: Box::new((Expr::Constant(Literal::Int(2)), 0..0)),
    }, 0..0));

    let stmt_id = translate_stmt(&mut buffer, stmt, &mut scope, &mut const_dir);

    println!("{}", String::from_utf8(buffer.into_inner().unwrap()).unwrap());
    println!("{:?}", const_dir);
    assert_eq!(stmt_id, "t2");
  }
}
