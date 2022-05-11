use std::{fmt::Display, io::{BufWriter, Write}, sync::Mutex};

use lazy_static::lazy_static;

use crate::directory::{Dir, self, Key};

use super::{parser::ast::{Decl, Expr, Literal, Type, Stmt, Block}, semantics::{item::{Item, Kind}, Scope}};

// #[derive(Debug, Clone)]
// struct Constant {
//   id: String,
//   value: Literal,
// }
// impl Constant {
//   fn new(id: String, value: Literal) -> Self {
//     Constant {
//       id,
//       value,
//     }
//   }
// }
// impl Key for Constant {
//     fn key(&self) -> &str {
//         &self.id
//     }
// }

lazy_static! {
  static ref COUNTER: Mutex<usize> = Mutex::new(0);
}

fn new_id() -> String {
  let mut counter = COUNTER.lock().unwrap();
  let id = format!("{}", counter);
  *counter += 1;
  id
}

#[derive(Debug, Clone, PartialEq)]
pub struct Quad {
    op: String,
    arg1: String,
    arg2: String,
    arg3: String,
}

impl Quad {
    fn new(op: &str, arg1: &str, arg2: &str, arg3: &str) -> Self {
        Quad {
            op: op.to_string(),
            arg1: arg1.to_string(),
            arg2: arg2.to_string(),
            arg3: arg3.to_string(),
        }
    }
}

impl Display for Quad {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let w = 15;
      write!(f, "{:^w$},{:^w$},{:^w$},{:^w$}", self.op, self.arg1, self.arg2, self.arg3)
  }
}

#[derive(Debug, Clone)]
pub struct Quadruples(Vec<Quad>);
impl Display for Quadruples {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    
    for (i, quad) in self.0.iter().enumerate() {
      write!(f, "{:>4} {}\n", i, quad)?;
    }

    Ok(())
  }
}
// fn map_dir_errs(err: directory::Error) -> Error {
//     match err {
//         directory::Error::Nonexistent(key) => Error::Nonexistent(key),
//         directory::Error::Duplicate(key) => Error::Duplicate(key),
//     }
//   }

fn translate_expr(output: &mut Vec<Quad>, expr: Expr) -> String {
  match expr {
    Expr::Binary { lhs, op, rhs } => {
      let lhs_res = translate_expr(output, lhs.0);
      let rhs_res = translate_expr(output, rhs.0);
  
      // let lhs_res = lhs.last().unwrap().arg3;
      // let rhs_res = rhs.last().unwrap().arg3;

      let dest = format!("t{}", new_id());
      
      let this = Quad {
        op: op.to_string(),
        arg1: lhs_res,
        arg2: rhs_res,
        arg3: dest.clone(),
      };

      output.push(this);

      dest
    }
    Expr::Ident(name) => name,
    Expr::Constant(literal) => literal.to_string(),

    Expr::Error => todo!(),
    Expr::Call { fun, args } => todo!(),
    Expr::Unary { op, rhs } => {
      let rhs_res = translate_expr(output, rhs.0);

      let dest = format!("t{}", new_id());

      let this = Quad {
        op: op.to_string(),
        arg1: rhs_res,
        arg2: "".to_string(),
        arg3: dest.clone(),
      };

      output.push(this);

      dest
    },
    Expr::Array(_) => todo!(),
    Expr::Parenthesized(inner) => translate_expr(output, inner.0),
    _ => todo!(),
  }
}

fn translate_stmt(output: &mut Vec<Quad>, stmt: Stmt) -> String {
   
  match stmt {
    Stmt::Decl(decl) => match decl {
        Decl::Let { name, type_: _, value } => {
          if let Some(value) = value {

            let value_id = translate_expr(output, value.0);

            let quad = Quad {
              op: "=".to_string(),
              arg1: value_id,
              arg2: "".to_string(),
              arg3: name.clone(),
            };

            output.push(quad);

            name
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
      let value_id = translate_expr(output, value.0);

      let quad = Quad {
        op: "=".to_string(),
        arg1: value_id,
        arg2: "".to_string(),
        arg3: to.to_string(),
      };

      output.push(quad);

      to.to_string()
    },
    Stmt::Cond { if_, then, elif, else_ } => {

      // ifs and else-ifs are handled in the same way
      let ifs = [vec![(if_, then)], elif].concat();

      let mut goto_pointers: Vec<usize> = Vec::new();
      for (if_, then) in ifs {

        let if_id = translate_expr(output, if_.0);

        let gotof_quad = Quad {
          op: "GOTOF".to_string(),
          arg1: if_id,
          arg2: "".to_string(),
          arg3: "".to_string(),
        };
        
        output.push(gotof_quad);
        let if_pointer = output.len() - 1; // GOTOF instruction index

        // translate inner block
        for (stmt, _) in then {
          let _ = translate_stmt(output, stmt);
        }

        let goto_end = Quad {
          op: "GOTO".to_string(),
          arg1: "".to_string(),
          arg2: "".to_string(),
          arg3: "".to_string(),
        };
        
        output.push(goto_end);
        goto_pointers.push(output.len() - 1); // we need to update the goto location when we finish the else
        
        let end_if_pointer = output.len(); // next instruction index

        output[if_pointer].arg2 = format!("{}", end_if_pointer);
      }

      if let Some(else_) = else_ {
        for (stmt, _) in else_ {
          let _ = translate_stmt(output, stmt);
        }
      }

      for goto in goto_pointers {
        output[goto].arg2 = format!("{}", output.len()); // update the goto location
      }

      "".to_string()
    },
    Stmt::Loop(_) => todo!(),
    Stmt::Expr((expr, span)) => translate_expr(output, expr),
    Stmt::Return(_) => todo!(),
    Stmt::Error => unreachable!(),
  }
}

pub(crate) fn translate(stmts: Block) -> Result<Quadruples, ()> {
  
  let mut output = Vec::new();

  for (stmt, _) in stmts {
    let _ = translate_stmt(&mut output, stmt);
  }
    
  output.push(Quad {
    op: "END".to_string(),
    arg1: "".to_string(),
    arg2: "".to_string(),
    arg3: "".to_string(),
  });

  let result = Quadruples(output);

  Ok(result)
}

mod tests {

  use chumsky::{Parser, Stream};

use crate::compiler::{parser::{ast::BinOp, parser}, lexer::lexer, semantics};

  use super::*;

  fn translate_from_src(src: &str) -> Result<Quadruples, ()> {
    let tokens = lexer().parse(src).unwrap();
    let len = tokens.len();
    let token_stream = Stream::from_iter(len..len + 1, tokens.into_iter());
    let ast = parser().parse(token_stream).unwrap();

    if let Err(errs) = semantics::semantic_analysis(ast.clone()) {
        println!("semantic errors: {:?}", errs);
        return Err(());
    }
        
    // Translate only if there were no semantic errors
    let res = translate(ast).unwrap();

    println!("{}", res.clone());

    Ok(res)
  }

  #[test]
  fn test_expressions() {
    let src = "
      1 + 2 * 3 || 4 / 5 && 6 > 7;
      ";

    let Quadruples(quads) = translate_from_src(src).unwrap();

    assert_eq!(quads.len(), 7);
    
    assert_eq!(quads[0], Quad::new("MUL", "i:2", "i:3", "t0"));
    assert_eq!(quads[1], Quad::new("ADD", "i:1", "t0", "t1"));
    assert_eq!(quads[2], Quad::new("DIV", "i:4", "i:5", "t2"));
    assert_eq!(quads[3], Quad::new("GT", "i:6", "i:7", "t3"));
    assert_eq!(quads[4], Quad::new("AND", "t2", "t3", "t4"));
    assert_eq!(quads[5], Quad::new("OR", "t1", "t4", "t5"));
  }

  #[test]
  fn test_if_else() {
    let src = "
      if 1 > 2 {
        3 + 4;
      } else {
        5 - 6;
      }
      ";

    let Quadruples(quads) = translate_from_src(src).unwrap();

    assert_eq!(quads.len(), 6);

    assert_eq!(quads[0], Quad::new("GT", "i:1", "i:2", "t0"));
    assert_eq!(quads[1], Quad::new("GOTOF", "t0", "4", ""));
    assert_eq!(quads[2], Quad::new("ADD", "i:3", "i:4", "t1"));
    assert_eq!(quads[3], Quad::new("GOTO", "", "5", ""));
    assert_eq!(quads[4], Quad::new("SUB", "i:5", "i:6", "t2"));
    assert_eq!(quads[5], Quad::new("END", "", "", ""));
  }

  #[test]
  fn test_if_elseif_else() {
    let src = "
      if 1 > 2 {
        3 + 4;
      } else if 5 > 6 {
        7 + 8;
      } else {
        9 - 10;
      }
      ";

    let Quadruples(quads) = translate_from_src(src).unwrap();

    assert_eq!(quads.len(), 10);

    assert_eq!(quads[0], Quad::new("GT",    "i:1", "i:2",  "t0"));
    assert_eq!(quads[1], Quad::new("GOTOF", "t0",  "4",    ""));
    assert_eq!(quads[2], Quad::new("ADD",   "i:3", "i:4",  "t1"));
    assert_eq!(quads[3], Quad::new("GOTO",  "",    "9",    ""));
    assert_eq!(quads[4], Quad::new("GT",    "i:5", "i:6",  "t2"));
    assert_eq!(quads[5], Quad::new("GOTOF", "t2",  "8",    ""));
    assert_eq!(quads[6], Quad::new("ADD",   "i:7", "i:8",  "t3"));
    assert_eq!(quads[7], Quad::new("GOTO",  "",    "9",    ""));
    assert_eq!(quads[8], Quad::new("SUB",   "i:9", "i:10", "t4"));
    assert_eq!(quads[9], Quad::new("END",   "",    "",     ""));
  }

  #[test]
  fn nested_if() {
    let src = "
      if 1 > 2 {
        if 3 > 4 {
          5 + 6;
        } else {
          7 - 8;
        }
      } else if 1.2 > 2.3 {
        9 - 10;
      } else {
        11 + 12;
      }
      ";

    let Quadruples(quads) = translate_from_src(src).unwrap();

    assert_eq!(quads.len(), 14);

    assert_eq!(quads[0], Quad::new("GT",    "i:1",  "i:2",    "t0"));
    assert_eq!(quads[1], Quad::new("GOTOF", "t0",   "8",      ""));
    assert_eq!(quads[2], Quad::new("GT",    "i:3",  "i:4",    "t1"));
    assert_eq!(quads[3], Quad::new("GOTOF", "t1",   "6",      ""));
    assert_eq!(quads[4], Quad::new("ADD",   "i:5",  "i:6",    "t2"));
    assert_eq!(quads[5], Quad::new("GOTO",  "",     "7",      ""));
    assert_eq!(quads[6], Quad::new("SUB",   "i:7",  "i:8",    "t3"));
    assert_eq!(quads[7], Quad::new("GOTO",  "",     "13",     ""));
    assert_eq!(quads[8], Quad::new("GT",    "f:1.2", "f:2.3", "t4"));
    assert_eq!(quads[9], Quad::new("GOTOF", "t4",    "12",    ""));
    assert_eq!(quads[10], Quad::new("SUB",   "i:9",  "i:10",  "t5"));
    assert_eq!(quads[11], Quad::new("GOTO",  "",     "13",    ""));
    assert_eq!(quads[12], Quad::new("ADD",   "i:11", "i:12",  "t6"));
    assert_eq!(quads[13], Quad::new("END",   "",     "",      ""));
  }
}
