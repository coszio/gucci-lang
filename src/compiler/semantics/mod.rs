
/// Implementation of the `Item` struct to be used in the scope directory.
pub mod item;

/// Implementation of the semantic considerations for binary operations.
pub mod semantic_cube;

/// Implementation of the semantic considerations for unary operations.
pub mod semantic_square;


use std::fmt::{Debug, Display};
use lazy_static::lazy_static;
use crate::shared::{Spanned, Span};
use crate::utils::lazy_counter::Counter;
use crate::utils::directory::{self, Dir};
use self::item::{Kind, Item};
use super::{parser::ast::*, semantics::semantic_cube::resolve};

pub(crate) type Scope = Dir<Item>;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Error {
  TypeMismatch(String, Type, Type),
  Undefined(String),
  Redefined(String),
  IncompatibleBinOp(BinOp, Type, Type),
  IncompatibleUnOp(UnOp, Type),
  UntypedVariable(String),
  NotAVariable(String, Kind),
  NotAFunction(String, Kind),
  ArgumentsMismatch(String, usize, usize),
  HeterogenousArray,
  EmptyArray,
  NonBooleanCondition(Type),
}

impl Error { 
  pub(crate) fn msg(&self) -> String {
    match self {
      Error::Undefined(key) => format!("`{}` does not exist in this scope", key),
      Error::TypeMismatch(key, type1, type2) => format!("`{}` has type {}, but is being assigned to {}", key, type1, type2),
      Error::Redefined(key) => format!("`{}` already exists in this scope", key),
      Error::NotAVariable(key, kind) => format!("`{}` is a {}, which is not assignable", key, kind),
      Error::IncompatibleBinOp(op, lhs_type, rhs_type) => format!("Operator `{}` is not compatible with {} and {}", op, lhs_type, rhs_type),
      Error::IncompatibleUnOp(op, rhs_type) => format!("Operator `{}` is not compatible with {}", op, rhs_type),
      Error::HeterogenousArray => format!("Arrays must have all its elements of the same type"),
      Error::EmptyArray => format!("Arrays must have at least one element"),
      Error::UntypedVariable(id) => format!("{} doesn't have a type, all variables must have a type", id),
      Error::NotAFunction(name, kind) => format!("`{}` is a {}, which is not a function", name, kind),
      Error::ArgumentsMismatch(name, params, args) => format!("`{}` takes {} arguments, but {} were given", name, params, args),
      Error::NonBooleanCondition(type_) => format!("Conditions must evaluate to a boolean, this evaluates to {}", type_),
    }
  }
}

impl Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Error::Undefined(..) => write!(f, "Undefined identifier"),
      Error::TypeMismatch(..) => write!(f, "Type mismatch"),
      Error::Redefined(..) => write!(f, "Redefined identifier"),
      Error::NotAVariable(..) => write!(f, "Not a variable"),
      Error::IncompatibleBinOp(..) => write!(f, "Invalid binary operation"),
      Error::IncompatibleUnOp(..) => write!(f, "Invalid unary operation"),
      Error::HeterogenousArray => write!(f, "Heterogenous array"),
      Error::EmptyArray => write!(f, "Empty array"),
      Error::UntypedVariable(..) => write!(f, "Variable without a type"),
      Error::NotAFunction(..) => write!(f, "Not a function"),
      Error::ArgumentsMismatch(..) => write!(f, "Argument mismatch"),
      Error::NonBooleanCondition(..) => write!(f, "Non-boolean condition"),
    }
  }
}

lazy_static! {
  static ref VAR_COUNTER: Counter = Counter::new("v");
  static ref FUN_COUNTER: Counter = Counter::new("f");
}
fn reset_counters() {
  VAR_COUNTER.reset();
  FUN_COUNTER.reset();
}

pub(self) type Result<T> = std::result::Result<T, Spanned<Error>>;

/// Maps the errors originated in [`Dir`] to semantic errors
fn map_dir_errs(err: directory::Error) -> Error {
  match err {
    directory::Error::Nonexistent(id) => Error::Undefined(id),
    directory::Error::Duplicate(id) => Error::Redefined(id),
  }
}

/// Evaluates expressions and replaces identifiers with their ids from the scope (in place).
/// Returns the resulting type of the expression.
pub(crate) fn eval_expr(expr: &mut Expr, span: Span, scope: &Scope) -> Result<Type> {
  
  match expr {
    Expr::Error => unreachable!(),
    Expr::Call { fun, args } => {
        let fun_name = match fun.0.clone() {
          Expr::Ident(name) => name,
          _ => panic!("Expected function name"),
        };

        let item = scope.get(&fun_name.clone()).map_err(|e| (map_dir_errs(e), span.clone()))?;

        let fun_type = item.type_.clone();

        // if it is a function it must have a vector of parameters, even if it is empty
        let params = match item.kind.clone() {
          Kind::Fun(params) => params,
          _ => return Err((Error::NotAFunction(item.name.clone(), item.kind.clone()), span)),
        };

        // swap name with stored id
        fun.0 = Expr::Ident(item.id.clone());

        // evaluate arguments
        if args.len() != params.len() {
          return Err((Error::ArgumentsMismatch(fun_name.clone(), params.len(), args.len()), span));
        }
        for ((arg, s), (param, _)) in args.iter_mut().zip(params.iter()) {
            let arg_type = eval_expr(arg, s.clone(), scope)?;

            // Check types
            if arg_type != param.type_ {
              return Err((Error::TypeMismatch(param.name.clone(), param.type_.clone(), arg_type.clone()), s.clone()));
            }
        }

        Ok(fun_type.unwrap_or_else(|| todo!("function calls without return type are not supported yet")))
    },
    Expr::Binary { lhs, op, rhs } => {
      let lhs_type = eval_expr(&mut lhs.0, lhs.1.clone(), scope)?;
      let rhs_type = eval_expr(&mut rhs.0, rhs.1.clone(), scope)?;
      
      resolve(&lhs_type, &op, &rhs_type).map_err(|_| (Error::IncompatibleBinOp(op.clone(), lhs_type, rhs_type), span))
    },
    Expr::Unary { op, rhs } => {
      let rhs_type = eval_expr(&mut rhs.0, rhs.1.clone(), scope)?;

      semantic_square::resolve(&op, &rhs_type).map_err(|_| (Error::IncompatibleUnOp(op.clone(), rhs_type), span))
    },
    Expr::Constant(literal) => match literal {
        Literal::Int(_) => Ok(Type::Int),
        Literal::Float(_) => Ok(Type::Float),
        Literal::Bool(_) => Ok(Type::Bool),
        Literal::Char(_) => Ok(Type::Char),
        Literal::String(_) => Ok(Type::String),
    },
    Expr::Ident(name) => {
      let item = scope.get(name.as_str()).map_err(|e| (map_dir_errs(e), span.clone()))?;
      if item.kind != Kind::Var {
        return Err((Error::NotAVariable(name.to_string(), item.kind.clone()), span.clone()));
      }
      let result = item.type_.clone().ok_or_else(|| (Error::UntypedVariable(name.to_string()), span.clone()));
      *name = item.id.clone();
      result
    },
    // Expr::Array(exprs) => {
      
    //   // Empty array declarations are not allowed
    //   if exprs.is_empty() {
    //     return Err((Error::EmptyArray, span.clone()));
    //   }

    //   // Evaluate items in the array
    //   let mut types = Vec::new();
    //   for (mut expr, span) in exprs {
    //     types.push(eval_expr(&mut expr, span.clone(), scope)?);
    //   }

    //   // All must be of the same type
    //   if types.iter().any(|t| *t != types[0]) {
    //       return Err((Error::HeterogenousArray, span.clone()));
    //   }

    //   Ok(Type::Array(Box::new(types[0].clone())))
    // },
    _ => todo!(),
  }

}

/// Evaluates statements and replaces identifiers with their ids from the scope.
/// Returns a copy of the statement, but with the ids replaced.
pub(crate) fn eval_stmt(stmt: Stmt, span: Span, scope: &mut Scope) -> Result<Spanned<Stmt>> {

  match stmt {

    Stmt::Decl(decl) => match decl {

        Decl::Let { name, type_, mut value } => {
          if let Some(value) = value.as_mut() {
            let value_type = eval_expr(&mut value.0, value.1.clone(), &scope)?;
            if type_ != value_type {
              return Err((Error::TypeMismatch(name.to_string(), type_.clone(), value_type), span.clone()));
            }
          }

          let new_item =Item::new(VAR_COUNTER.new_id(), name.clone(), Kind::Var, type_.clone());

          scope
            .create(new_item.clone())
            .map_err(|e| (map_dir_errs(e), span.clone()))?;

          
          Ok((Stmt::Decl(Decl::Let { name: new_item.id, type_, value: value.clone() }), span))
        },

        Decl::Fun(Fun { name, mut params, ret_type, mut body }) => {

          let new_fun = Item { 
            id: FUN_COUNTER.new_id(), 
            name: name.clone(), 
            kind: Kind::Fun(params.clone()), 
            type_: ret_type.clone(),
          };

          scope.create(new_fun.clone()).map_err(|e| (map_dir_errs(e), span.clone()))?;

          let mut subscope = scope.add_child();

          for param in params.iter_mut() {
            let new_item = Item::new(
              VAR_COUNTER.new_id(), 
              param.0.name.clone(),
              Kind::Var, 
              param.0.type_.clone());

            subscope
              .create(new_item.clone())
              .map_err(|e| (map_dir_errs(e), span.clone()))?;
            
            param.0.name = new_item.id;
          }

          for (stmt, span) in body.iter_mut() {
            // Return expressions only work in the root scope of the function
            if let Stmt::Return((mut expr, span)) = stmt.clone() {
              let res_type = eval_expr(&mut expr, span.clone(), &subscope)?;

                if let Some(ret_type) = ret_type.clone() {
                  if res_type != ret_type {
                    return Err((Error::TypeMismatch(name.to_string(), ret_type, res_type), span.clone()));
                  }
                }
            }
            (*stmt, _) = eval_stmt(stmt.clone(), span.clone(), &mut subscope)?;
            
          }

          *scope = subscope.drop();

          let name = new_fun.id.clone();

          Ok((Stmt::Decl(Decl::Fun(Fun { name, params, ret_type, body })), span))
        },

        Decl::Class { name, inherits, implements, has, does } => todo!(),

        Decl::Interface { name, should_do } => todo!(),
    },

    // Stmt::Assign { to, value } => {

    //   let value_type = eval_expr(&mut value.0, value.1, &scope)?;
    //   let to_type = scope
    //     .get(to.to_string().as_str())
    //     .map_err(|e| (map_dir_errs(e), span.clone()))?
    //     .type_
    //     .clone()
    //     .expect("Expressions should have a type");

    //   if value_type != to_type {
    //     return Err((Error::TypeMismatch(to.to_string(), to_type, value_type), span.clone()));
    //   }

    //   let item = scope.get(to.to_string().as_str()).map_err(|e| (map_dir_errs(e), span.clone()))?;

    //   if item.kind != Kind::Var {
    //     return Err((Error::NotAVariable(to.to_string(), item.kind.clone()), span.clone()));
    //   }

    //   let item_type = item.type_.clone().expect("Variables must always have a type");
    //   if item_type != value_type {
    //     return Err((Error::TypeMismatch(to.to_string(), item_type, value_type), span.clone()));
    //   }
      
    //   *to = item.id.clone();

    //   Ok(())

    // },

    Stmt::Cond { if_, then, elif, mut else_ } => {
      let mut ifs = [vec![(if_, then)], elif].concat();

      for (if_, then) in ifs.iter_mut() {
        let if_type = eval_expr(&mut if_.0, if_.1.clone(), &scope)?;
        if if_type != Type::Bool {
          return Err((Error::NonBooleanCondition(if_type), span.clone()));
        }

        let mut subscope = scope.add_child();

        for (stmt, span) in then.iter_mut() {
          (*stmt, _) = eval_stmt(stmt.clone(), span.clone(), &mut subscope)?;
        }

        *scope = subscope.drop();

      }
      
      if let Some(else_) = else_.as_mut() {
        let mut subscope = scope.add_child();

        for (stmt, span) in else_.iter_mut() {
          (*stmt, _) = eval_stmt(stmt.clone(), span.clone(), &mut subscope)?;
        }

        *scope = subscope.drop();
      }

      // Destructure into original shape
      match &ifs[..] {
        [(if_, then), elif @ ..] =>
            Ok((Stmt::Cond { 
              if_: if_.clone(),
              then: then.clone(), 
              elif: elif.to_vec(), 
              else_ }, span)),
        
        _ => unreachable!("Conditionals should always have at least one element"),
      }
    },

    Stmt::Loop(loop_) => match loop_ {
        Loop::While { mut cond, mut body } => {
          let cond_type = eval_expr(&mut cond.0, cond.1.clone(), &scope)?;

          if cond_type != Type::Bool {
            return Err((Error::NonBooleanCondition(cond_type), span.clone()));
          }

          let mut subscope = scope.add_child();

          for (stmt, span) in body.iter_mut() {
            (*stmt, _) = eval_stmt(stmt.clone(), span.clone(), &mut subscope)?;
          }

          *scope = subscope.drop();


          Ok((Stmt::Loop(Loop::While { cond, body }), span))
        },
    },

    Stmt::Expr((mut e, s)) => {
      eval_expr(&mut e, s.clone(), &scope)?;
      Ok((Stmt::Expr((e, s)), span))
    },

    
    Stmt::Return((mut e, s)) => { 
      // return expressions can only be in the root scope of a function, for now
      eval_expr(&mut e, s.clone(), &scope)?; 
      Ok((Stmt::Return((e, s)), span))
    },

    Stmt::Print((mut e, s)) => {
      eval_expr(&mut e, s.clone(), &scope)?;
      Ok((Stmt::Print((e, s)), span))
    },
    
    Stmt::Error => todo!(),
    _ => todo!(),
  }
}


pub(crate) fn semantic_analysis(stmts: Block) -> std::result::Result<Block, Vec<Spanned<Error>>> {
  reset_counters();
  let mut scope = Scope::new();
  let result: Vec<Result< Spanned<Stmt> >> = stmts
    .iter()
    .map(|(stmt, span)| eval_stmt(stmt.clone(), span.clone(), &mut scope))
    .collect();
    
  // Extract errors
  let errors: Vec<Spanned<Error>> = result.clone()
    .into_iter()
    .filter_map(|res| res.err())
    .collect();

  if errors.is_empty() {
    // Extract correct statements
    let stmts: Block = result
      .into_iter()
      .filter_map(|res| res.ok())
      .collect();

    return Ok(stmts)
  }

  Err(errors)
}

#[cfg(test)]
mod tests {

  use crate::compiler::{lexer::lexer, parser::parser};

use super::*;
  use chumsky::{Parser, Stream};
use serial_test::serial; // we need this because we have global counters, tests would fail in parallel

  fn analyze_from_src(src: &str) -> std::result::Result<Block, Vec<Spanned<Error>>> {
    let tokens = lexer().parse(src).unwrap();
    let len = tokens.len();
    let token_stream = Stream::from_iter(len..len + 1, tokens.into_iter());
    let ast = parser().parse(token_stream).unwrap();
    semantic_analysis(ast)
  }

  #[test]
  #[serial]
  fn test_eval_expr() {
    reset_counters();
    
    let scope = Scope::new();
    let mut expr = Expr::Binary {
      lhs: Box::new((Expr::Constant(Literal::Int(1)), 0..0)),
      op: BinOp::Add,
      rhs: Box::new((Expr::Constant(Literal::Int(2)), 0..0)),
    };
    let result = eval_expr(&mut expr, 0..0, &scope);
    assert_eq!(result, Ok(Type::Int));
  }

  #[test]
  #[serial]
  fn test_eval_expr_with_scope() {
    reset_counters();
    
    let mut scope = Scope::new();
    scope.create(Item::new("v0".to_string(), "x".to_string(), Kind::Var, Type::Int)).unwrap();
    let mut expr = Expr::Ident("x".to_string());

    let result = eval_expr(&mut expr, 0..0, &scope);

    assert_eq!(result, Ok(Type::Int));
  }

  #[test]
  #[serial]
  fn test_decl_var() {
    let block = vec![
        (Stmt::Decl(Decl::Let {
          name: "x".to_string(),
          type_: Type::Int,
          value: Some((Expr::Constant(Literal::Int(1)), 0..0)),
        }), 0..0),

        (Stmt::Decl(Decl::Let {
          name: "y".to_string(),
          type_: Type::String,
          value: None,
        }), 0..0),
      ];

    let result = semantic_analysis(block);
    assert_eq!(result, Ok( vec![
        (Stmt::Decl(Decl::Let {
          name: "v0".to_string(),
          type_: Type::Int,
          value: Some((Expr::Constant(Literal::Int(1)), 0..0)),
        }), 0..0),

        (Stmt::Decl(Decl::Let {
          name: "v1".to_string(),
          type_: Type::String,
          value: None,
        }), 0..0),
      ])
    );
  }

  #[test]
  #[serial]
  fn test_decl_fun() {
    reset_counters();
    let mut scope = Scope::new();
    let stmt = Stmt::Decl(Decl::Fun(Fun{
      name: "f".to_string(),
      params: vec![(Var {name: "x".to_string(), type_: Type::Int}, 0..0)],
      ret_type: Some(Type::Int),
      body: vec![(Stmt::Return((Expr::Constant(Literal::Int(1)), 0..0)), 0..0)],
    }));

    let result = eval_stmt(stmt, 0..0, &mut scope);
    assert_eq!(
      result, 
      Ok(
        (Stmt::Decl(Decl::Fun(Fun{
          name: "f0".to_string(),
          params: vec![(Var {name: "v0".to_string(), type_: Type::Int}, 0..0)],
          ret_type: Some(Type::Int),
          body: vec![(Stmt::Return((Expr::Constant(Literal::Int(1)), 0..0)), 0..0)],
        })), 0..0)
      )
    );
  }

  #[test]
  #[serial]
  fn test_scopes() {
    let src = "
      let x: int = 1;
      if x { 
        let y: int = 2;
        let z: int = x + y;
      }
      print z;
      ";

    let result = analyze_from_src(src);
    println!("{:?}", result);
    assert!(result.is_err());
  }
}