
use std::fmt::{Debug, Display};

use crate::{
  directory::{self, Scope, Kind, Item}, 
  parser::{Stmt, Decl, Expr, Loop, Type, Literal, BinOp, UnOp, Fun, Block}, 
  lexer::Span,
  semantic_cube::resolve,
};

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
}

impl Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Error::Undefined(key) => write!(f, "Error: `{}` does not exist in this scope", key),
      Error::TypeMismatch(key, type1, type2) => write!(f, "Error: `{}` has type {}, but is being assigned to {}", key, type1, type2),
      Error::Redefined(key) => write!(f, "Error: `{}` already exists in this scope", key),
      Error::NotAVariable(key, kind) => write!(f, "Error: `{}` is a {}, which is not assignable", key, kind),
      Error::Incompatible(op, lhs_type, rhs_type) => write!(f, "Error: `{:?}` is not compatible with {} and {}", op, lhs_type, rhs_type),
      Error::HeterogenousArray => write!(f, "Error: Arrays must have all its elements of the same type"),
      Error::EmptyArray => write!(f, "Error: Arrays must have at least one element"),
      Error::UntypedVariable(id) => write!(f, "Error: {} doesn't have a type, all variables must have a type", id),
      Error::NotAFunction(_, _) => todo!(),
    }
  }
}

type Result<T> = std::result::Result<T, Error>;
fn map_dir_errs(err: directory::Error) -> Error {
  match err {
    directory::Error::MismatchingTypes(id, l_type, r_type) => Error::TypeMismatch(id, l_type, r_type),
    directory::Error::Nonexistent(id) => Error::Undefined(id),
    directory::Error::Duplicate(id) => Error::Redefined(id),
    directory::Error::Unassignable(id, kind) => Error::NotAVariable(id, kind),
  }
}

fn eval_expr(expr: Expr, span: Span, scope: &Scope) -> Result<Type> {
  // let scope = directory::Scope::new();
  match expr {
    Expr::Error => todo!(),
    Expr::Call { fun, args } => todo!(),
    Expr::Binary { lhs, op, rhs } => {
      let lhs_type = eval_expr(lhs.0, lhs.1, scope)?;
      let rhs_type = eval_expr(rhs.0, rhs.1, scope)?;
      
      resolve(&lhs_type, &op, &rhs_type).map_err(|_| Error::Incompatible(op, lhs_type, rhs_type))
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
      let item = scope.get(id.as_str()).map_err(map_dir_errs)?;
      if item.kind != Kind::Var {
        return Err(Error::NotAVariable(id.to_string(), item.kind.clone()));
      }
      item.type_.clone().ok_or_else(|| Error::UntypedVariable(id.to_string()))
    },
    Expr::Array(exprs) => {
      
      // Empty array declarations are not allowed
      if exprs.is_empty() {
        return Err(Error::EmptyArray);
      }

      // Evaluate items in the array
      let mut types = Vec::new();
      for expr in exprs {
        types.push(eval_expr(expr.0, expr.1, scope)?);
      }

      // All must be of the same type
      if types.iter().any(|t| *t != types[0]) {
          return Err(Error::HeterogenousArray);
      }

      Ok(Type::Array(Box::new(types[0].clone())))
    },
    Expr::Parenthesized(e) => eval_expr(e.0, e.1, scope),
  }

}


fn eval_stmt(stmt: Stmt, span: Span, scope: &mut Scope) -> Result<()> {

  match stmt {

    Stmt::Decl(decl) => match decl {

        Decl::Let { name, type_, value } => {
          if let Some(value) = value {
            let value_type = eval_expr(value.0, value.1, &scope)?;
            if type_ != value_type {
              return Err(Error::TypeMismatch(name.to_string(), type_, value_type));
            }
          }
          scope.create(Item::new(name, Kind::Var, type_)).map_err(map_dir_errs)
        },

        Decl::Fun(Fun { name, params, ret_type, body }) => {

          let mut subscope = scope.add_child();
          
          for param in params {
            subscope
              .create(Item::new(param.0.name, Kind::Var, param.0.type_))
            .map_err(map_dir_errs)?;
          }

          for (stmt, span) in body {
            eval_stmt(stmt, span, &mut subscope)?;
          }

          *scope = subscope.drop();

          scope.create(Item { id: name, kind: Kind::Fun, type_: ret_type }).map_err(map_dir_errs)
        },

        Decl::Class { name, inherits, implements, has, does } => todo!(),

        Decl::Interface { name, should_do } => todo!(),
    },

    Stmt::Assign { to, value } => {

      let value_type = eval_expr(value.0, value.1, &scope)?;
      let to_type = scope
        .get(to.to_string().as_str())
        .map_err(map_dir_errs)?
        .type_
        .clone()
        .ok_or_else(|| panic!("Expressions should have a type"))?;

      if value_type != to_type {
        return Err(Error::TypeMismatch(to.to_string(), to_type, value_type));
      }

      scope.update(Item::new(to.to_string(), Kind::Var, value_type)).map_err(map_dir_errs)
    },

    Stmt::Cond { if_, then, elif, else_ } => todo!(),

    Stmt::Loop(loop_) => match loop_ {
        Loop::While { cond, body } => todo!(),
    },

    Stmt::Expr((expr, span)) => {
      eval_expr(expr, span, &scope)?;
      Ok(())
    },

    Stmt::Error => todo!(),

    Stmt::Return((expr, span)) => todo!(),
  }
}


pub(crate) fn semantic_analysis(stmts: Block) -> std::result::Result<(), Vec<Error>> {

  let mut scope = directory::Scope::new();
  let errors: Vec<Error> = stmts
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

  use crate::parser::Var;

use super::*;

  #[test]
  fn test_eval_expr() {
    let scope = directory::Scope::new();
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
    let mut scope = directory::Scope::new();
    scope.create(Item::new("x".to_string(), Kind::Var, Type::Int)).unwrap();
    let expr = Expr::Ident("x".to_string());
    let result = eval_expr(expr, 0..0, &scope);
    assert_eq!(result, Ok(Type::Int));
  }

  #[test]
  fn test_decl_var() {
    let mut scope = directory::Scope::new();
    let stmt = Stmt::Decl(Decl::Let {
      name: "x".to_string(),
      type_: Type::Int,
      value: Some((Expr::Constant(Literal::Int(1)), 0..0)),
    });

    let result = eval_stmt(stmt, 0..0, &mut scope);
    assert_eq!(result, Ok(()));

    let mut scope = directory::Scope::new();
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
    let mut scope = directory::Scope::new();
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