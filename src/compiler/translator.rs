use std::{fmt::Display, io::{BufWriter, Write}, sync::Mutex, collections::HashMap};

use lazy_static::lazy_static;

use crate::{directory::{Dir, self, Key}, compiler::parser::ast::Loop, utils::lazy_counter::Counter};

use super::{parser::ast::{Decl, Expr, Literal, Type, Stmt, Block, Fun}, semantics::{item::{Item, Kind}, Scope}};

lazy_static! {
  static ref TEMP_COUNTER: Counter = Counter::new("t");
  static ref CONST_COUNTER: Counter = Counter::new("c");
}
const W: usize = 14;

fn reset_counters() {
  TEMP_COUNTER.reset();
  CONST_COUNTER.reset();
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
      write!(f, "{:^W$},{:^W$},{:^W$},{:^W$}", self.op, self.arg1, self.arg2, self.arg3)
  }
}

#[derive(Debug, Clone)]
struct Function {
  id: String,
  pointer: usize,
  params: Vec<(String, Type)>,
  ret_type: Option<Type>,
}
impl Function {
  fn size(&self) -> usize {
    self.params.iter().map(|(_, t)| t.size()).sum()
  }
}
impl Display for Function {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:^W$},{:^W$}", self.id, self.pointer)?;
    Ok(())
  }
}

#[derive(Debug, Clone)]
struct Const {
  id: String,
  literal: Literal,
}
impl Display for Const {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:^W$}, {:^W$}, {:^W$}", "CONST", self.id, self.literal)
  }
}
#[derive(Clone)]
pub struct BigSheep {
  funcs: Vec<Function>,
  consts: Vec<Const>,
  quads: Vec<Quad>,
}
impl BigSheep {
  fn new() -> Self {
    BigSheep {
      funcs: Vec::new(),
      consts: Vec::new(),
      quads: Vec::new(),
    }
  }
}
impl Display for BigSheep {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    
    for func in &self.funcs {
      write!(f, "{}\n", func)?;
    }
    write!(f, "%%\n")?;
    for const_ in &self.consts {
      write!(f, "{}\n", const_)?;
    }
    write!(f, "%%\n")?;
    for (i, quad) in self.quads.iter().enumerate() {
      write!(f, "{:>4} {}\n", i, quad)?;
    }

    Ok(())
  }
}


fn translate_expr(output: &mut BigSheep, expr: Expr) -> String {
  match expr {
    Expr::Binary { lhs, op, rhs } => {
      let lhs_res = translate_expr(output, lhs.0);
      let rhs_res = translate_expr(output, rhs.0);

      let dest = TEMP_COUNTER.new_id();
      
      let this = Quad {
        op: op.to_string(),
        arg1: lhs_res,
        arg2: rhs_res,
        arg3: dest.clone(),
      };

      output.quads.push(this);

      dest
    }
    Expr::Ident(name) => name,
    Expr::Constant(literal) => {

      let id = CONST_COUNTER.new_id();

      output.consts.push(Const {
        id: id.clone(),
        literal,
      });

      id
    },

    Expr::Error => todo!(),
    Expr::Call { fun, args } => {
      let fun_id = match fun.0 {
        Expr::Ident(id) => id,
        _ => panic!("Expected function name"),
      };

      let func = output.funcs.iter().find(|f| f.id == fun_id).unwrap().clone();

      for (i, arg) in args.iter().enumerate() {
        let arg_id = translate_expr(output, arg.0.clone());
        let par_id = format!("p{}", i);
        output.quads.push(Quad {
          op: "=".to_string(),
          arg1: arg_id,
          arg2: "".to_string(),
          arg3: par_id,
        });
      }

      output.quads.push(Quad::new("GOSUB", &func.id, &func.pointer.to_string(), ""));

      let fun_return = output.quads[func.pointer].arg3.clone();
      let call_return = TEMP_COUNTER.new_id();
      
      output.quads.push(Quad::new("=", &fun_return, "", &call_return.clone()));
      call_return
    },
    Expr::Unary { op, rhs } => {
      let rhs_res = translate_expr(output, rhs.0);

      let dest = TEMP_COUNTER.new_id();

      let this = Quad {
        op: op.to_string(),
        arg1: rhs_res,
        arg2: "".to_string(),
        arg3: dest.clone(),
      };

      output.quads.push(this);

      dest
    },
    Expr::Array(_) => todo!(),
    Expr::Parenthesized(inner) => translate_expr(output, inner.0),
    _ => todo!(),
  }
}

fn translate_stmt(output: &mut BigSheep, stmt: Stmt) -> String {
   
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

            output.quads.push(quad);

            name
          }
          else {
            todo!();
          }
        },
        Decl::Fun(Fun { name, params, ret_type, body }) => {
          
          // skip function if reading from before
          output.quads.push(Quad::new("GOTO", "", "", ""));
          let goto_ptr = output.quads.len() - 1;
          
          // declare new Function
          let mut fun = Function {
            id: name.clone(),
            pointer: output.quads.len(),
            params: Vec::new(),
            ret_type: ret_type.clone(),
          };

          // setup return
          let mut ret_dest: String = String::new();
          if let Some(_) = ret_type {
            let dest = TEMP_COUNTER.new_id();
            output.quads.push(Quad::new("RETURN", "", "", &dest));
            ret_dest = dest;
          }

          output.quads.push(Quad::new("BEGINBLOCK", "", "", ""));

          // allocate resources
          for (param, _) in params.iter() {
             fun.params.push((param.name.clone(), param.type_.clone()));
          }
          output.quads.push(Quad::new("ERA", &(*fun.size().to_string()), "", ""));

          for (i, (name, _)) in fun.params.iter().enumerate() {
            output.quads.push(Quad {
              op: "PARAM".to_string(), 
              arg1: format!("p{i}"), 
              arg2: "".to_string(), 
              arg3: name.clone(),
            });
          }

          output.funcs.push(fun);

          // generate quads
          for (stmt, _) in body {
            match stmt {
              Stmt::Return((expr, _)) => {
                let expr_id = translate_expr(output, expr);
                output.quads.push(Quad::new("=", &expr_id, "", &ret_dest));
                break;
              },
              _ => translate_stmt(output, stmt),
            };
          }

          // end function
          output.quads.push(Quad{
            op: "ENDBLOCK".to_string(),
            arg1: "".to_string(),
            arg2: "".to_string(),
            arg3: "".to_string(),
          });
          output.quads[goto_ptr].arg2 = output.quads.len().to_string(); // set GOTO to point to end of function
          "".to_string()
        },
        Decl::Class { name, inherits, implements, has, does } => todo!(),
        Decl::Interface { name, should_do } => todo!(),
    },
    // Stmt::Assign { to, value } => {
    //   let value_id = translate_expr(output, value.0);

    //   let quad = Quad {
    //     op: "=".to_string(),
    //     arg1: value_id,
    //     arg2: "".to_string(),
    //     arg3: to.to_string(),
    //   };

    //   output.quads.push(quad);

    //   to.to_string()
    // },
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
        
        output.quads.push(gotof_quad);
        let if_pointer = output.quads.len() - 1; // GOTOF instruction index

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
        
        output.quads.push(goto_end);
        goto_pointers.push(output.quads.len() - 1); // we need to update the goto location when we finish the else
        
        let end_if_pointer = output.quads.len(); // next instruction index

        output.quads[if_pointer].arg2 = end_if_pointer.to_string();
      }

      if let Some(else_) = else_ {
        for (stmt, _) in else_ {
          let _ = translate_stmt(output, stmt);
        }
      }

      for goto in goto_pointers {
        output.quads[goto].arg2 = format!("{}", output.quads.len()); // update the goto location
      }

      "".to_string()
    },
    Stmt::Loop(loop_) => match loop_ {
      Loop::While { cond, body } => {

        let cond_pointer = output.quads.len(); // we need to reevaluate the expression after the loop
        let condition_id = translate_expr(output, cond.0);

        let gotof_quad = Quad {
          op: "GOTOF".to_string(),
          arg1: condition_id,
          arg2: "".to_string(),
          arg3: "".to_string(),
        };

        output.quads.push(gotof_quad);
        let gotof_pointer = output.quads.len() - 1; // GOTOF instruction index

        // translate inner block
        for (stmt, _) in body {
          let _ = translate_stmt(output, stmt);
        }

        // return to the condition of the loop
        let goto_cond = Quad {
          op: "GOTO".to_string(),
          arg1: "".to_string(),
          arg2: cond_pointer.to_string(),
          arg3: "".to_string(),
        };
        output.quads.push(goto_cond);

        output.quads[gotof_pointer].arg2 = output.quads.len().to_string(); // update the gotof location to exit the loop
        "".to_string()
      },
    },
    Stmt::Expr((expr, _)) => translate_expr(output, expr),
    Stmt::Return((expr, _)) => translate_expr(output, expr),
    Stmt::Error => unreachable!(),
  }
}

pub(crate) fn translate(stmts: Block) -> Result<BigSheep, ()> {
  
  reset_counters();

  let mut output = BigSheep::new();

  for (stmt, _) in stmts {
    let _ = translate_stmt(&mut output, stmt);
  }
    
  output.quads.push(Quad {
    op: "END".to_string(),
    arg1: "".to_string(),
    arg2: "".to_string(),
    arg3: "".to_string(),
  });

  let result = output;

  Ok(result)
}

mod tests {

  use chumsky::{Parser, Stream};

use crate::compiler::{parser::{parser}, lexer::lexer, semantics};

  use super::*;

  fn translate_from_src(src: &str) -> Result<BigSheep, ()> {
    let tokens = lexer().parse(src).unwrap();
    let len = tokens.len();
    let token_stream = Stream::from_iter(len..len + 1, tokens.into_iter());
    let ast = parser().parse(token_stream).unwrap();


    match semantics::semantic_analysis(ast.clone()) {
      Ok(ast) => {
        let res = translate(ast).unwrap();
        println!("{}", res);
        Ok(res)
      },
      Err(errs) => {
        println!("semantic errors found: {:?}", errs);
        Err(())
      }
    }

    // if let Err(errs) = semantics::semantic_analysis(ast.clone()) {
    //     println!("semantic errors: {:?}", errs);
    //     return Err(());
    // }
        
    // Translate only if there were no semantic errors
    // let res = translate(ast).unwrap();

    // println!("{}", res.clone());

    // Ok(res)
  }

  #[test]
  fn test_expressions() {
    let src = "
      1 + 2 * 3 || 4 / 5 && 6 > 7;
      ";

    let BigSheep { funcs, consts, quads } = translate_from_src(src).unwrap();

    assert_eq!(quads.len(), 7);
    
    assert_eq!(quads[0], Quad::new("MUL", "c1", "c2", "t0"));
    assert_eq!(quads[1], Quad::new("ADD", "c0", "t0", "t1"));
    assert_eq!(quads[2], Quad::new("DIV", "c3", "c4", "t2"));
    assert_eq!(quads[3], Quad::new("GT", "c5", "c6", "t3"));
    assert_eq!(quads[4], Quad::new("AND", "t2", "t3", "t4"));
    assert_eq!(quads[5], Quad::new("OR", "t1", "t4", "t5"));
  }

  #[test]
  fn test_assignment() {
    let src = "
      let a: int = 1;
      let b: int = a + 2;
      let c: int = b * 3;
      ";

    let BigSheep { funcs, consts, quads } = translate_from_src(src).unwrap();

    // assert_eq!(quads.len(), 6);
    
    assert_eq!(quads[0], Quad::new("=", "c0", "", "v0"));
    assert_eq!(quads[1], Quad::new("ADD", "v0", "c1", "t0"));
    assert_eq!(quads[2], Quad::new("=", "t0", "", "v1"));
    assert_eq!(quads[3], Quad::new("MUL", "v1", "c2", "t1"));
    assert_eq!(quads[4], Quad::new("=", "t1", "", "v2"));
    assert_eq!(quads[5], Quad::new("END", "", "", ""));
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

    let BigSheep { funcs, consts, quads } = translate_from_src(src).unwrap();

    assert_eq!(quads.len(), 6);

    assert_eq!(quads[0], Quad::new("GT", "c0", "c1", "t0"));
    assert_eq!(quads[1], Quad::new("GOTOF", "t0", "4", ""));
    assert_eq!(quads[2], Quad::new("ADD", "c2", "c3", "t1"));
    assert_eq!(quads[3], Quad::new("GOTO", "", "5", ""));
    assert_eq!(quads[4], Quad::new("SUB", "c4", "c5", "t2"));
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

    let BigSheep { funcs, consts, quads }  = translate_from_src(src).unwrap();

    assert_eq!(quads.len(), 10);

    assert_eq!(quads[0], Quad::new("GT",    "c0", "c1",  "t0"));
    assert_eq!(quads[1], Quad::new("GOTOF", "t0",  "4",    ""));
    assert_eq!(quads[2], Quad::new("ADD",   "c2", "c3",  "t1"));
    assert_eq!(quads[3], Quad::new("GOTO",  "",    "9",    ""));
    assert_eq!(quads[4], Quad::new("GT",    "c4", "c5",  "t2"));
    assert_eq!(quads[5], Quad::new("GOTOF", "t2",  "8",    ""));
    assert_eq!(quads[6], Quad::new("ADD",   "c6", "c7",  "t3"));
    assert_eq!(quads[7], Quad::new("GOTO",  "",    "9",    ""));
    assert_eq!(quads[8], Quad::new("SUB",   "c8", "c9", "t4"));
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

    let BigSheep { funcs, consts, quads }  = translate_from_src(src).unwrap();

    assert_eq!(quads.len(), 14);

    assert_eq!(quads[0], Quad::new("GT",    "c0",  "c1",    "t0"));
    assert_eq!(quads[1], Quad::new("GOTOF", "t0",   "8",      ""));
    assert_eq!(quads[2], Quad::new("GT",    "c2",  "c3",    "t1"));
    assert_eq!(quads[3], Quad::new("GOTOF", "t1",   "6",      ""));
    assert_eq!(quads[4], Quad::new("ADD",   "c4",  "c5",    "t2"));
    assert_eq!(quads[5], Quad::new("GOTO",  "",     "7",      ""));
    assert_eq!(quads[6], Quad::new("SUB",   "c6",  "c7",    "t3"));
    assert_eq!(quads[7], Quad::new("GOTO",  "",     "13",     ""));
    assert_eq!(quads[8], Quad::new("GT",    "c8", "c9", "t4"));
    assert_eq!(quads[9], Quad::new("GOTOF", "t4",    "12",    ""));
    assert_eq!(quads[10], Quad::new("SUB",   "c10",  "c11",  "t5"));
    assert_eq!(quads[11], Quad::new("GOTO",  "",     "13",    ""));
    assert_eq!(quads[12], Quad::new("ADD",   "c12", "c13",  "t6"));
    assert_eq!(quads[13], Quad::new("END",   "",     "",      ""));
  }

  #[test]
  fn test_while() {
    let src = "
      while 1 > 2 {
        3 + 4;
      }
    ";

    let BigSheep { funcs, consts, quads }  = translate_from_src(src).unwrap();

    assert_eq!(quads.len(), 5);
      
    assert_eq!(quads[0], Quad::new("GT", "c0", "c1", "t0"));
    assert_eq!(quads[1], Quad::new("GOTOF", "t0", "4", ""));
    assert_eq!(quads[2], Quad::new("ADD", "c2", "c3", "t1"));
    assert_eq!(quads[3], Quad::new("GOTO", "", "0", ""));
    assert_eq!(quads[4], Quad::new("END", "", "", ""));
  }

  #[test]
  fn test_fun_decl() {
    let src = r#"
      2 * 3;
      fun foo(x: int, y: int): string {
        x + y + 40;
        return "hello";
      }
    "#;

    let BigSheep { funcs, consts, quads }  = translate_from_src(src).unwrap();

    assert_eq!(funcs.len(), 1);
    assert_eq!(funcs[0].pointer, 2);
    assert_eq!(funcs[0].params.len(), 2);
    assert_eq!(funcs[0].params[0].0, "v0");
    assert_eq!(funcs[0].params[0].1, Type::Int);
    assert_eq!(funcs[0].params[1].0, "v1");
    assert_eq!(funcs[0].params[1].1, Type::Int);
  }

  #[test]
  fn test_fun_call() {
    let src = r#"
      fun foo(x: int, y: int): string {
        return "hello";
      }
      let s: string = foo(1, 2);
    "#;

    let BigSheep { funcs, consts, quads }  = translate_from_src(src).unwrap();

    assert_eq!(quads[8], Quad::new("=", "c1", "", "p0"));
    assert_eq!(quads[9], Quad::new("=", "c2", "", "p1"));
    assert_eq!(quads[10], Quad::new("GOSUB", "", "1", ""));
    assert_eq!(quads[11], Quad::new("=", "t0", "", "v2"));
  }

  #[test]
  fn test_recursive_call() {

      let src = r#"
        fun fibonacci(n: int): int {
          let ans: int = 0;
          if n == 1 {
            ans = 1;
          } else if n > 1 {
            ans = fibonacci(n - 1) + fibonacci(n - 2);
          }
          return ans;
        }"#;

      let BigSheep { funcs, consts, quads }  = translate_from_src(src).unwrap();

      assert_eq!(quads.len(), 13);
      assert_eq!(quads[0], Quad::new("FUNC", "2", "2", ""));
      assert_eq!(quads[1], Quad::new("PARAM", "v0", "", ""));
      assert_eq!(quads[2], Quad::new("IF", "c0", "", ""));
      assert_eq!(quads[3], Quad::new("=", "c1", "", "p0"));
      assert_eq!(quads[4], Quad::new("GOTO", "", "6", ""));
      assert_eq!(quads[5], Quad::new("=", "c2", "", "p1"));
      assert_eq!(quads[6], Quad::new("GOTO", "", "8", ""));
      assert_eq!(quads[7], Quad::new("=", "c3", "", "p2"));
      assert_eq!(quads[8], Quad::new("GOTO", "", "10", ""));
      assert_eq!(quads[9], Quad::new("=", "c4", "", "p3"));
      assert_eq!(quads[10], Quad::new("GOTO", "", "12", ""));
      assert_eq!(quads[11], Quad::new("=", "c5", "", "p4"));
      assert_eq!(quads[12], Quad::new("END", "", "", ""));

      
  }
}
