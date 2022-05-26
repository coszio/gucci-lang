use std::fmt::Display;

use crate::{utils::directory::Key, compiler::parser::ast::{Type, Var}, shared::Spanned};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Item { 
  pub id: String,
  pub name: String,
  pub kind: Kind,
  pub type_: Option<Type>,
}

impl Item {
  pub fn new(id: String, name: String, kind: Kind, type_: Type) -> Self {
    Item {
      id,
      name,
      kind,
      type_: Some(type_),
    }
  }
}

impl Key for Item {
  fn key(&self) -> &str {
    &self.name
  }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Kind {
  Literal,
  Var,
  Fun(Vec<Spanned<Var>>),
  Class,
  Interface,
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Var => write!(f, "variable"),
            Kind::Fun(_) => write!(f, "function"),
            Kind::Class => write!(f, "class"),
            Kind::Interface => write!(f, "interface"),
            Kind::Literal => write!(f, "literal"),
        }
    }
}

#[cfg(test)]
mod tests {

  use crate::{compiler::semantics::Scope, utils::directory::{ Result, Error }};

  use super::*;

  #[test]
  fn test_create() {
    let mut scope = Scope::new();

    let r1 = scope.create(Item { 
      id: "v0".to_string(),
      name: "a".to_string(),
      kind: Kind::Var, 
      type_: Some(Type::Int)
    });
    assert!(r1.is_ok());

    let r2 = scope.create(Item{ 
      id: "v1".to_string(),
      name: "foo".to_string(),
      kind: Kind::Fun(vec![]), 
      type_: None
    });
    assert!(r2.is_ok());

    let r3 = scope.create(Item{ 
      id: "f0".to_string(),
      name: "foo".to_string(),
      kind: Kind::Var, 
      type_: None
    });
    assert_eq!(r3, Err(Error::Duplicate("foo".to_string())));
  }

  #[test]
  fn test_get() -> Result<()> {

    let mut scope = Scope::new();

    scope.create(Item{ 
      id: "v0".to_string(),
      name: "a".to_string(),
      kind: Kind::Var, 
      type_: Some(Type::Int)
    })?;

    scope.create(Item{ 
      id: "f0".to_string(),
      name: "foo".to_string(),
      kind: Kind::Fun(vec![]), 
      type_: None
    })?;

    assert!(scope.get("a").is_ok());
    assert!(scope.get("foo").is_ok());
    assert_eq!(scope.get("bar"), Err(Error::Nonexistent("bar".to_string())));
    assert_eq!(scope.get("a")?.type_, Some(Type::Int));

    Ok(())
  }

  #[test]
  fn test_has() -> Result<()> {

    let mut scope = Scope::new();

    scope.create(Item{ 
      id: "v0".to_string(),
      name: "a".to_string(),
      kind: Kind::Var, 
      type_: Some(Type::Int)
    })?;

    scope.create(Item{ 
      id: "f0".to_string(),
      name: "foo".to_string(),
      kind: Kind::Fun(vec![]), 
      type_: Some(Type::Float)
    })?;

    assert!(scope.has("a"));
    assert!(scope.has("foo"));
    assert!(!scope.has("bar"));

    Ok(())
  }


  #[test]
  fn test_remove() -> Result<()> {

    let mut scope = Scope::new();

    scope.create(Item {
      id: "v0".to_string(),
      name: "a".to_string(), 
      kind: Kind::Var, 
      type_: Some(Type::Int)
    })?;

    scope.create(Item { 
      id: "f0".to_string(),
      name: "foo".to_string(),
      kind: Kind::Fun(vec![]), 
      type_: Some(Type::Float)
    })?;

    scope.remove("a")?;
    scope.remove("foo")?;

    assert_eq!(scope.get("a"), Err(Error::Nonexistent("a".to_string())));
    assert_eq!(scope.get("foo"), Err(Error::Nonexistent("foo".to_string())));

    Ok(())
  }

  #[test]
  fn test_nested_scope() -> Result<()> {

    let mut scope = Scope::new();

    scope.create(Item {
      id: "v0".to_string(),
      name: "a".to_string(), 
      kind: Kind::Var, 
      type_: Some(Type::Int)
    })?;

    scope.create(Item { 
      id: "f0".to_string(),
      name: "foo".to_string(),
      kind: Kind::Fun(vec![]), 
      type_: Some(Type::Float)
    })?;
    
    
    let mut nested = scope.add_child();
    
    assert!(nested.get("a").is_ok());
    assert!(nested.get("foo").is_ok());
    
    nested.create(Item {
      id: "v1".to_string(),
      name: "a".to_string(), 
      kind: Kind::Var, 
      type_: Some(Type::String)
    })?;

    nested.create(Item { 
      id: "f1".to_string(),
      name: "foo".to_string(),
      kind: Kind::Class, 
      type_: None
    })?;

    assert_eq!(
      nested.get("a"),
      Ok(&Item {
        id: "v1".to_string(),
        name: "a".to_string(), 
        kind: Kind::Var, 
        type_: Some(Type::String)
      }));
    assert_eq!(
      nested.get("foo"), 
      Ok(&Item { 
        id: "f1".to_string(),
        name: "foo".to_string(),
        kind: Kind::Class, 
        type_: None
      }));

    // return ownership to the parent
    scope = nested.drop();

    assert_eq!(
      scope.get("a"), 
      Ok(&Item {
        id: "v0".to_string(),
        name: "a".to_string(), 
        kind: Kind::Var, 
        type_: Some(Type::Int)
      }));

    Ok(())
  }

}