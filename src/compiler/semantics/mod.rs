
pub mod item;
pub mod semantic_cube;
pub mod semantic_square;

use std::{fmt::{Debug, Display}, ops::Range};

use crate::directory::{self, Dir};

use self::item::{Kind, Item};

use super::{parser::ast::*, semantics::semantic_cube::resolve, lexer::Span};

pub(crate) type Scope = Dir<Item>;

// static mut SCOPE: directory::Scope = directory::Scope::new();
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Error {
  TypeMismatch(String, Type, Type),
  Undefined(String),
  Redefined(String),
  Incompatible(BinOp, Type, Type),
  UntypedVariable(String),
  NotAVariable(String, Kind),
  NotAFunction(String, Kind),
  HeterogenousArray,
  EmptyArray,
  NonBooleanCondition(Type),
}

impl Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Error::Undefined(key) => write!(f, "`{}` does not exist in this scope", key),
      Error::TypeMismatch(key, type1, type2) => write!(f, "`{}` has type {}, but is being assigned to {}", key, type1, type2),
      Error::Redefined(key) => write!(f, "`{}` already exists in this scope", key),
      Error::NotAVariable(key, kind) => write!(f, "`{}` is a {}, which is not assignable", key, kind),
      Error::Incompatible(op, lhs_type, rhs_type) => write!(f, "`{:?}` is not compatible with {} and {}", op, lhs_type, rhs_type),
      Error::HeterogenousArray => write!(f, "Arrays must have all its elements of the same type"),
      Error::EmptyArray => write!(f, "Arrays must have at least one element"),
      Error::UntypedVariable(id) => write!(f, "{} doesn't have a type, all variables must have a type", id),
      Error::NotAFunction(_, _) => todo!(),
      Error::NonBooleanCondition(type_) => write!(f, "Conditions must return a boolean, this is returning {}", type_),
    }
  }
}

pub(self) type Result<T> = std::result::Result<T, Spanned<Error>>;

fn map_dir_errs(err: directory::Error) -> Error {
  match err {
    directory::Error::MismatchingTypes(id, l_type, r_type) => Error::TypeMismatch(id, l_type, r_type),
    directory::Error::Nonexistent(id) => Error::Undefined(id),
    directory::Error::Duplicate(id) => Error::Redefined(id),
    directory::Error::Unassignable(id, kind) => Error::NotAVariable(id, kind),
  }
}

pub(crate) fn eval_expr(expr: Expr, span: Span, scope: &Scope) -> Result<Type> {
  
  match expr {
    Expr::Error => unreachable!(),
    Expr::Call { fun, args } => todo!(),
    Expr::Binary { lhs, op, rhs } => {
      let lhs_type = eval_expr(lhs.0, lhs.1, scope)?;
      let rhs_type = eval_expr(rhs.0, rhs.1, scope)?;
      
      resolve(&lhs_type, &op, &rhs_type).map_err(|_| (Error::Incompatible(op, lhs_type, rhs_type), span))
    },
    Expr::Unary { op, rhs } => todo!(),
    Expr::Constant(literal) => match literal {
        Literal::Int(_) => Ok(Type::Int),
        Literal::Float(_) => Ok(Type::Float),
        Literal::Bool(_) => Ok(Type::Bool),
        Literal::Char(_) => Ok(Type::Char),
        Literal::String(_) => Ok(Type::String),
    },
    Expr::Ident(id) => {
      let item = scope.get(id.as_str()).map_err(|e| (map_dir_errs(e), span.clone()))?;
      if item.kind != Kind::Var {
        return Err((Error::NotAVariable(id.to_string(), item.kind.clone()), span.clone()));
      }
      item.type_.clone().ok_or_else(|| (Error::UntypedVariable(id.to_string()), span.clone()))
    },
    Expr::Array(exprs) => {
      
      // Empty array declarations are not allowed
      if exprs.is_empty() {
        return Err((Error::EmptyArray, span.clone()));
      }

      // Evaluate items in the array
      let mut types = Vec::new();
      for (expr, span) in exprs {
        types.push(eval_expr(expr, span.clone(), scope)?);
      }

      // All must be of the same type
      if types.iter().any(|t| *t != types[0]) {
          return Err((Error::HeterogenousArray, span.clone()));
      }

      Ok(Type::Array(Box::new(types[0].clone())))
    },
    Expr::Parenthesized(e) => eval_expr(e.0, e.1, scope),
  }

}


pub(crate) fn eval_stmt(stmt: Stmt, span: Span, scope: &mut Scope) -> Result<()> {

  match stmt {

    Stmt::Decl(decl) => match decl {

        Decl::Let { name, type_, value } => {
          if let Some(value) = value {
            let value_type = eval_expr(value.0, value.1, &scope)?;
            if type_ != value_type {
              return Err((Error::TypeMismatch(name.to_string(), type_, value_type), span.clone()));
            }
          }
          scope.create(Item::new(name, Kind::Var, type_)).map_err(|e| (map_dir_errs(e), span.clone()))
        },

        Decl::Fun(Fun { name, params, ret_type, body }) => {

          let mut subscope = scope.add_child();
          
          for param in params {
            subscope
              .create(Item::new(param.0.name, Kind::Var, param.0.type_))
            .map_err(|e| (map_dir_errs(e), span.clone()))?;
          }

          for (stmt, span) in body {
            eval_stmt(stmt, span.clone(), &mut subscope)?;
          }

          *scope = subscope.drop();

          scope.create(Item { id: name, kind: Kind::Fun, type_: ret_type }).map_err(|e| (map_dir_errs(e), span.clone()))
        },

        Decl::Class { name, inherits, implements, has, does } => todo!(),

        Decl::Interface { name, should_do } => todo!(),
    },

    Stmt::Assign { to, value } => {

      let value_type = eval_expr(value.0, value.1, &scope)?;
      let to_type = scope
        .get(to.to_string().as_str())
        .map_err(|e| (map_dir_errs(e), span.clone()))?
        .type_
        .clone()
        .expect("Expressions should have a type");

      if value_type != to_type {
        return Err((Error::TypeMismatch(to.to_string(), to_type, value_type), span.clone()));
      }

      scope.update(Item::new(to.to_string(), Kind::Var, value_type)).map_err(|e| (map_dir_errs(e), span.clone()))
    },

    Stmt::Cond { if_, then, elif, else_ } => {
      let ifs = [vec![(if_, then)], elif].concat();

      for (if_, then) in ifs {
        let if_type = eval_expr(if_.0, if_.1, &scope)?;
        if if_type != Type::Bool {
          return Err((Error::NonBooleanCondition(if_type), span.clone()));
        }

        for (stmt, span) in then {
          eval_stmt(stmt, span.clone(), scope)?;
        }
      }

      if let Some(else_) = else_ {
        for (stmt, span) in else_ {
          eval_stmt(stmt, span.clone(), scope)?;
        }
      }

      Ok(())
    },

    Stmt::Loop(loop_) => match loop_ {
        Loop::While { cond, body } => {
          let cond_type = eval_expr(cond.0, cond.1, &scope)?;

          if cond_type != Type::Bool {
            return Err((Error::NonBooleanCondition(cond_type), span.clone()));
          }

          for (stmt, span) in body {
            eval_stmt(stmt, span.clone(), scope)?;
          }

          Ok(())
        },
    },

    Stmt::Expr((expr, span)) => {
      eval_expr(expr, span, &scope)?;
      Ok(())
    },

    Stmt::Error => todo!(),

    Stmt::Return((expr, span)) => todo!(),
  }
}


pub(crate) fn semantic_analysis(stmts: Block) -> std::result::Result<(), Vec<Spanned<Error>>> {

  let mut scope = Scope::new();
  let errors: Vec<Spanned<Error>> = stmts
    .iter()
    .map(|(stmt, span)| eval_stmt(stmt.clone(), span.clone(), &mut scope))
    .filter_map(|res| res.err())
    .collect();

  if errors.is_empty() {
    return Ok(())
  }

  Err(errors)
}

#[cfg(test)]
mod tests {

use super::*;

  #[test]
  fn test_eval_expr() {
    let scope = Scope::new();
    let expr = Expr::Binary {
      lhs: Box::new((Expr::Constant(Literal::Int(1)), 0..0)),
      op: BinOp::Add,
      rhs: Box::new((Expr::Constant(Literal::Int(2)), 0..0)),
    };
    let result = eval_expr(expr, 0..0, &scope);
    assert_eq!(result, Ok(Type::Int));
  }

  #[test]
  fn test_eval_expr_with_scope() {
    let mut scope = Scope::new();
    scope.create(Item::new("x".to_string(), Kind::Var, Type::Int)).unwrap();
    let expr = Expr::Ident("x".to_string());
    let result = eval_expr(expr, 0..0, &scope);
    assert_eq!(result, Ok(Type::Int));
  }

  #[test]
  fn test_decl_var() {
    let mut scope = Scope::new();
    let stmt = Stmt::Decl(Decl::Let {
      name: "x".to_string(),
      type_: Type::Int,
      value: Some((Expr::Constant(Literal::Int(1)), 0..0)),
    });

    let result = eval_stmt(stmt, 0..0, &mut scope);
    assert_eq!(result, Ok(()));

    let mut scope = Scope::new();
    let stmt = Stmt::Decl(Decl::Let {
      name: "x".to_string(),
      type_: Type::String,
      value: None,
    });

    let result = eval_stmt(stmt, 0..0, &mut scope);
    assert_eq!(result, Ok(()));
  }

  #[test]
  fn test_decl_fun() {
    let mut scope = Scope::new();
    let stmt = Stmt::Decl(Decl::Fun(Fun{
      name: "f".to_string(),
      params: vec![(Var {name: "x".to_string(), type_: Type::Int}, 0..0)],
      ret_type: Some(Type::Int),
      body: vec![(Stmt::Expr((Expr::Constant(Literal::Int(1)), 0..0)), 0..0)],
    }));

    let result = eval_stmt(stmt, 0..0, &mut scope);
    assert_eq!(result, Ok(()));
  }
}