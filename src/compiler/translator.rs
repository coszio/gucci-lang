use std::fmt::Display;

use lazy_static::lazy_static;

use crate::{
    compiler::parser::ast::Loop,
    shared::{op_code::OpCode, quad::Quad, W},
    utils::lazy_counter::Counter,
};

use super::parser::ast::{BinOp, Block, Decl, Expr, Fun, Literal, Stmt, Type};

lazy_static! {
    static ref TEMP_COUNTER: Counter = Counter::new("t");
    static ref CONST_COUNTER: Counter = Counter::new("c");
}

fn reset_counters() {
    TEMP_COUNTER.reset();
    CONST_COUNTER.reset();
}

#[derive(Clone)]
struct Function {
    id: String,
    pointer: usize,
    params: Vec<(String, Type)>,
    ret_type: Option<Type>,
    ret_dir: Option<String>,
}
impl Function {
    fn size(&self) -> usize {
        self.params.iter().map(|(_, t)| t.size()).sum()
    }
}
impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let type_ = match &self.ret_type { Some(t) => t.to_string(), None => "".to_string() };
        let ret_var = self.ret_dir.clone().unwrap_or_default();
        write!(f, "{:^W$},{:^W$},{:^W$},{:^W$}", self.id, self.pointer, type_, ret_var)?;
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
        write!(f, "{:^W$}, {:^W$}", self.id, self.literal)
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
            write!(f, "{:>4}.{}\n", i, quad)?;
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
                op: OpCode::try_from(op).unwrap(), // todo: handle chain operator
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
        }

        Expr::Error => todo!(),
        Expr::Call { fun, args } => {
            let fun_id = match fun.0 {
                Expr::Ident(id) => id,
                _ => panic!("Expected function name"),
            };

            let func = output
                .funcs
                .iter()
                .find(|f| f.id == fun_id)
                .unwrap()
                .clone();

            for (i, arg) in args.iter().enumerate() {
                let arg_id = translate_expr(output, arg.0.clone());
                let par_id = format!("p{}", i);
                output.quads.push(Quad {
                    op: OpCode::Param,
                    arg1: arg_id,
                    arg2: "".to_string(),
                    arg3: par_id,
                });
            }

            output.quads.push(Quad::new(
                OpCode::GoSub,
                &func.id,
                &func.pointer.to_string(),
                "",
            ));

            let call_return = TEMP_COUNTER.new_id();

            if let Some(fun_return) = func.ret_dir.clone() {
                output.quads.push(Quad::new(
                    OpCode::Assign,
                    &fun_return,
                    "",
                    &call_return.clone(),
                ));
                return call_return
            }
            "".to_string()
        }
        Expr::Unary { op, rhs } => {
            let rhs_res = translate_expr(output, rhs.0);

            let dest = TEMP_COUNTER.new_id();

            let this = Quad {
                op: OpCode::try_from(op).unwrap(),
                arg1: rhs_res,
                arg2: "".to_string(),
                arg3: dest.clone(),
            };

            output.quads.push(this);

            dest
        }
        Expr::Array(_) => todo!(),
        Expr::Parenthesized(inner) => translate_expr(output, inner.0),
        _ => todo!(),
    }
}

fn translate_stmt(output: &mut BigSheep, stmt: Stmt) -> String {
    match stmt {
        Stmt::Decl(decl) => match decl {
            Decl::Let { name, type_, value } => {
                // Update variables table
                match type_ {
                    // Primitive
                    Type::Int
                    | Type::Char
                    | Type::Bool
                    | Type::Float => output.quads.push(Quad {
                                        op: OpCode::NewVar,
                                        arg1: type_.to_string(),
                                        arg2: name.clone(),
                                        arg3: "".to_string(),
                                    }),

                    // Array
                    Type::Array(_) => todo!(),

                    // Other
                    _ => unimplemented!(),
                };
                
                if let Some(value) = value {
                    let value_id = translate_expr(output, value.0);

                    let quad = Quad {
                        op: OpCode::Assign,
                        arg1: value_id,
                        arg2: "".to_string(),
                        arg3: name.clone(),
                    };

                    output.quads.push(quad);
                } 

                "".to_string()
            }
            
            Decl::Fun(Fun {
                name,
                params,
                ret_type,
                body,
            }) => {
                // skip function if reading from before
                output.quads.push(Quad::new(OpCode::Goto, "", "", ""));
                let goto_ptr = output.quads.len() - 1;

                // declare new Function
                let mut fun = Function {
                    id: name.clone(),
                    pointer: output.quads.len(),
                    params: Vec::new(),
                    ret_type: ret_type.clone(),
                    ret_dir: None,
                };

                // setup return
                let mut ret_dest: String = String::new();
                if let Some(_) = ret_type {
                    let dest = TEMP_COUNTER.new_id();
                    fun.ret_dir = Some(dest.clone());
                    ret_dest = dest;
                }

                output.quads.push(Quad::new(OpCode::BeginBlock, "", "", ""));

                // allocate resources
                for (param, _) in params.iter() {
                    fun.params.push((param.name.clone(), param.type_.clone()));
                    output.quads.push(Quad {
                        op: OpCode::NewVar,
                        arg1: param.type_.to_string(),
                        arg2: param.name.clone(),
                        arg3: "".to_string(),
                    });
                }

                for (i, (name, _)) in fun.params.iter().enumerate() {
                    output.quads.push(Quad {
                        op: OpCode::Assign,
                        arg1: format!("p{i}"),
                        arg2: "".to_string(),
                        arg3: name.clone(),
                    });
                }

                output.funcs.push(fun);

                // generate quads
                let mut ret_goto_ptr = Option::<usize>::None;
                for (stmt, _) in body {
                    match stmt {
                        // TODO: handle nested returns
                        Stmt::Return((expr, _)) => {
                            let expr_id = translate_expr(output, expr);
                            if ret_type.is_some() {
                                output.quads.push(Quad::new(
                                    OpCode::Assign, 
                                    &expr_id,
                                    "", 
                                    &ret_dest));
                            }
                            output.quads.push(Quad::new(OpCode::Goto, "", "", ""));
                            ret_goto_ptr = Some(output.quads.len() - 1);
                            break;
                        }
                        _ => translate_stmt(output, stmt),
                    };
                }

                // end function
                output.quads.push(Quad {
                    op: OpCode::EndBlock,
                    arg1: "".to_string(),
                    arg2: "".to_string(),
                    arg3: "".to_string(),
                });
                if let Some(ret_goto_ptr) = ret_goto_ptr {
                    let endblock_ptr = output.quads.len() - 1;
                    output.quads[ret_goto_ptr].arg2 = endblock_ptr.to_string();
                }
                output.quads[goto_ptr].arg2 = output.quads.len().to_string(); // set GOTO to point to end of function
                "".to_string()
            }
            Decl::Class {
                name,
                inherits,
                implements,
                has,
                does,
            } => todo!(),
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
        Stmt::Cond {
            if_,
            then,
            elif,
            else_,
        } => {
            // ifs and else-ifs are handled in the same way
            let ifs = [vec![(if_, then)], elif].concat();

            let mut goto_pointers: Vec<usize> = Vec::new();
            for (if_, then) in ifs {
                let if_id = translate_expr(output, if_.0);

                let gotof_quad = Quad {
                    op: OpCode::GotoF,
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
                    op: OpCode::Goto,
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
        }
        Stmt::Loop(loop_) => match loop_ {
            Loop::While { cond, body } => {
                let cond_pointer = output.quads.len(); // we need to reevaluate the expression after the loop
                let condition_id = translate_expr(output, cond.0);

                let gotof_quad = Quad {
                    op: OpCode::GotoF,
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
                    op: OpCode::Goto,
                    arg1: "".to_string(),
                    arg2: cond_pointer.to_string(),
                    arg3: "".to_string(),
                };
                output.quads.push(goto_cond);

                output.quads[gotof_pointer].arg2 = output.quads.len().to_string(); // update the gotof location to exit the loop
                "".to_string()
            }
        },
        Stmt::Expr((expr, _)) => translate_expr(output, expr),
        Stmt::Return((expr, _)) => translate_expr(output, expr),
        Stmt::Print((expr, _)) => {
            let res = translate_expr(output, expr);
            output.quads.push(Quad {
                op: OpCode::Print,
                arg1: res,
                arg2: "".to_string(),
                arg3: "".to_string(),
            });
            "".to_string()
        }
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
        op: OpCode::End,
        arg1: "".to_string(),
        arg2: "".to_string(),
        arg3: "".to_string(),
    });

    let result = output;

    Ok(result)
}

#[cfg(test)]
mod tests {

    use std::error::Error;

    use chumsky::{Parser, Stream};

    use serial_test::serial;

    use crate::compiler::{lexer::lexer, parser::parser, semantics};

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
            }
            Err(errs) => {
                println!("semantic errors found: {:?}", errs);
                Err(())
            }
        }
    }

    fn assert_quads(quads: Vec<Quad>, expected_quads: Vec<&str>) {
        assert_eq!(quads.len(), expected_quads.len());
        for (i, quad) in quads.iter().enumerate() {
            assert_eq!(quad, &expected_quads[i].parse::<Quad>().unwrap());
        }
    }

    #[test]
    #[serial]
    fn test_expressions() {
        let src = "
      let a: bool = 1 + 2 * (-3) || 4 / 5 && 6 > 7;
      ";

        let BigSheep {
            funcs,
            consts,
            quads,
        } = translate_from_src(src).unwrap();

        assert_quads(quads, vec![
            "  NEWVAR   ,      bool    ,      v0      ,        ",
            "  NEG      ,      c2      ,              ,      t0",
            "  MUL      ,      c1      ,      t0      ,      t1",
            "  ADD      ,      c0      ,      t1      ,      t2",
            "  DIV      ,      c3      ,      c4      ,      t3",      
            "   GT      ,      c5      ,      c6      ,      t4",
            "  AND      ,      t3      ,      t4      ,      t5",
            "   OR      ,      t2      ,      t5      ,      t6",
            "    =      ,      t6      ,              ,      v0",
            "  END      ,              ,              ,        ",
        ]);
    }

    #[test]
    #[serial]
    fn test_assignment() {
        let src = "
      let a: int = 1;
      let b: int = a + 2;
      let c: int = b * 3;
      ";

        let BigSheep {
            funcs,
            consts,
            quads,
        } = translate_from_src(src).unwrap();

        assert_quads(quads, vec![
            "    NEWVAR    ,     int      ,      v0      ,              ",
            "      =       ,      c0      ,              ,      v0      ",
            "    NEWVAR    ,     int      ,      v1      ,              ",
            "     ADD      ,      v0      ,      c1      ,      t0      ",
            "      =       ,      t0      ,              ,      v1      ",
            "    NEWVAR    ,     int      ,      v2      ,              ",
            "     MUL      ,      v1      ,      c2      ,      t1      ",
            "      =       ,      t1      ,              ,      v2      ",
            "     END      ,              ,              ,              ",
        ]);
    }

    #[test]
    #[serial]
    fn test_if_else() {
        let src = "
      if 1 > 2 {
        3 + 4;
      } else {
        5 - 6;
      }
      ";

        let BigSheep {
            funcs,
            consts,
            quads,
        } = translate_from_src(src).unwrap();

        assert_quads(quads, vec![
            "      GT      ,      c0      ,      c1      ,      t0      ",
            "    GOTOF     ,      t0      ,      4       ,              ",
            "     ADD      ,      c2      ,      c3      ,      t1      ",
            "     GOTO     ,              ,      5       ,              ",
            "     SUB      ,      c4      ,      c5      ,      t2      ",
            "     END      ,              ,              ,              ",
        ]);
    }

    #[test]
    #[serial]
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

        let BigSheep {
            funcs,
            consts,
            quads,
        } = translate_from_src(src).unwrap();

        assert_quads(quads, vec![
            "      GT      ,      c0      ,      c1      ,      t0      ",
            "    GOTOF     ,      t0      ,      4       ,              ",
            "     ADD      ,      c2      ,      c3      ,      t1      ",
            "     GOTO     ,              ,      9       ,              ",
            "      GT      ,      c4      ,      c5      ,      t2      ",
            "    GOTOF     ,      t2      ,      8       ,              ",
            "     ADD      ,      c6      ,      c7      ,      t3      ",
            "     GOTO     ,              ,      9       ,              ",
            "     SUB      ,      c8      ,      c9      ,      t4      ",
            "     END      ,              ,              ,              ",
        ]);
    }

    #[test]
    #[serial]
    fn test_nested_if() {
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

        let BigSheep {
            funcs,
            consts,
            quads,
        } = translate_from_src(src).unwrap();

        assert_quads(quads, vec![
            "      GT      ,      c0      ,      c1      ,      t0",
            "    GOTOF     ,      t0      ,      8       ,",
            "      GT      ,      c2      ,      c3      ,      t1",
            "    GOTOF     ,      t1      ,      6       ,",
            "     ADD      ,      c4      ,      c5      ,      t2",
            "     GOTO     ,              ,      7       ,",
            "     SUB      ,      c6      ,      c7      ,      t3",
            "     GOTO     ,              ,      13      ,",
            "      GT      ,      c8      ,      c9      ,      t4",
            "    GOTOF     ,      t4      ,      12      ,",
            "     SUB      ,     c10      ,     c11      ,      t5",
            "     GOTO     ,              ,      13      ,",
            "     ADD      ,     c12      ,     c13      ,      t6",
            "     END      ,              ,              ,",
        ]);
    }

    #[test]
    #[serial]
    fn test_while() {
        let src = "
      while 1 > 2 {
        3 + 4;
      }
    ";

        let BigSheep {
            funcs,
            consts,
            quads,
        } = translate_from_src(src).unwrap();

        assert_quads(quads, vec![
            "      GT      ,      c0      ,      c1      ,      t0",
            "    GOTOF     ,      t0      ,      4       ,",
            "     ADD      ,      c2      ,      c3      ,      t1",
            "     GOTO     ,              ,      0       ,",
            "     END      ,              ,              ,",
        ]);
    }

    #[test]
    #[serial]
    fn test_fun_decl() {
        let src = r#"
      2 * 3;
      fun foo(x: int, y: int): string {
        x + y + 40;
        return "hello";
      }
    "#;

        let BigSheep {
            funcs,
            consts,
            quads,
        } = translate_from_src(src).unwrap();

        assert_eq!(funcs.len(), 1);
        assert_eq!(funcs[0].pointer, 2);
        assert_eq!(funcs[0].params.len(), 2);
        assert_eq!(funcs[0].params[0].0, "v0");
        assert_eq!(funcs[0].params[0].1, Type::Int);
        assert_eq!(funcs[0].params[1].0, "v1");
        assert_eq!(funcs[0].params[1].1, Type::Int);
    }

    #[test]
    #[serial]
    fn test_fun_call() {
        let src = r#"
      fun foo(x: int, y: int): float {
        return 2.5;
      }
      let s: float = foo(1, 2);
    "#;

        let BigSheep {
            funcs,
            consts,
            quads,
        } = translate_from_src(src).unwrap();
        assert_quads(quads, vec![
            "     GOTO     ,              ,      9       ,              ",
            "  BEGINBLOCK  ,              ,              ,              ",
            "    NEWVAR    ,     int      ,      v0      ,              ",
            "    NEWVAR    ,     int      ,      v1      ,              ",
            "      =       ,      p0      ,              ,      v0      ",
            "      =       ,      p1      ,              ,      v1      ",                
            "      =       ,      c0      ,              ,      t0      ",        
            "     GOTO     ,              ,      8       ,              ",        
            "   ENDBLOCK   ,              ,              ,              ",
            "    NEWVAR    ,    float     ,      v2      ,              ",
            "    PARAM     ,      c1      ,              ,      p0      ",
            "    PARAM     ,      c2      ,              ,      p1      ",
            "    GOSUB     ,      f0      ,      1       ,              ",
            "      =       ,      t0      ,              ,      t1      ",        
            "      =       ,      t1      ,              ,      v2      ",        
            "     END      ,              ,              ,              ",
        ]);
    }

    #[test]
    #[serial]
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

        let BigSheep {
            funcs,
            consts,
            quads,
        } = translate_from_src(src).unwrap();

        assert_quads(quads, vec![
            "     GOTO     ,              ,      26      ,        ",
            "  BEGINBLOCK  ,              ,              ,        ",
            "    NEWVAR    ,     int      ,      v0      ,        ",
            "      =       ,      p0      ,              ,      v0",       
            "    NEWVAR    ,     int      ,      v1      ,        ",
            "      =       ,      c0      ,              ,      v1",       
            "      EQ      ,      v0      ,      c1      ,      t1",       
            "    GOTOF     ,      t1      ,      10      ,        ",
            "      =       ,      v1      ,      c2      ,      t2",       
            "     GOTO     ,              ,      23      ,        ",
            "      GT      ,      v0      ,      c3      ,      t3",       
            "    GOTOF     ,      t3      ,      23      ,        ",
            "     SUB      ,      v0      ,      c4      ,      t4",       
            "    PARAM     ,      t4      ,              ,      p0",       
            "    GOSUB     ,      f0      ,      1       ,        ",
            "      =       ,      t0      ,              ,      t5",       
            "     SUB      ,      v0      ,      c5      ,      t6",       
            "    PARAM     ,      t6      ,              ,      p0",       
            "    GOSUB     ,      f0      ,      1       ,        ",
            "      =       ,      t0      ,              ,      t7",       
            "     ADD      ,      t5      ,      t7      ,      t8",       
            "      =       ,      v1      ,      t8      ,      t9",       
            "     GOTO     ,              ,      23      ,        ",
            "      =       ,      v1      ,              ,      t0",       
            "     GOTO     ,              ,      25      ,        ",       
            "   ENDBLOCK   ,              ,              ,        ",
            "     END      ,              ,              ,        ",
        ]);
    }
}
