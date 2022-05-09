use std::fmt::Display;

use crate::{directory::{Update, Error, Key, Result}, compiler::parser::ast::Type};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Item { 
  pub id: String,
  pub kind: Kind,
  pub type_: Option<Type>,
}

impl Item {
  pub fn new(id: String, kind: Kind, type_: Type) -> Self {
    Item {
      id,
      kind,
      type_: Some(type_),
    }
  }
}

impl Update<Item> for Item {
  fn update(&mut self, other: &Item) -> Result<()> {

    if self.kind != Kind::Var {
      return Err(Error::Unassignable(self.id.clone(), self.kind.clone()));
    }
    if let (Some(a), Some(b)) = (self.type_.as_ref(), other.type_.as_ref()) {
      if a != b {
        return Err(Error::MismatchingTypes(self.id.clone(), a.clone(), b.clone()))
      }
      return Ok(())
    }
    panic!("A variable must always have a type");
  }
}

impl Key for Item {
  fn key(&self) -> &str {
    &self.id
  }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Kind {
  Literal,
  Var,
  Fun,
  Class,
  Interface,
}

impl Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Kind::Var => write!(f, "variable"),
            Kind::Fun => write!(f, "function"),
            Kind::Class => write!(f, "class"),
            Kind::Interface => write!(f, "interface"),
            Kind::Literal => write!(f, "literal"),
        }
    }
}

#[cfg(test)]
mod tests {

  use crate::compiler::semantics::Scope;

use super::*;

  #[test]
  fn test_create() {
    let mut scope = Scope::new();

    let r1 = scope.create(Item{ 
      id: "a".to_string(),
      kind: Kind::Var, 
      type_: Some(Type::Int)
    });
    assert!(r1.is_ok());

    let r2 = scope.create(Item{ 
      id: "foo".to_string(),
      kind: Kind::Fun, 
      type_: None
    });
    assert!(r2.is_ok());

    let r3 = scope.create(Item{ 
      id: "foo".to_string(),
      kind: Kind::Var, 
      type_: None
    });
    assert_eq!(r3, Err(Error::Duplicate("foo".to_string())));
  }

  #[test]
  fn test_get() -> Result<()> {

    let mut scope = Scope::new();

    scope.create(Item{ 
      id: "a".to_string(),
      kind: Kind::Var, 
      type_: Some(Type::Int)
    })?;

    scope.create(Item{ 
      id: "foo".to_string(),
      kind: Kind::Fun, 
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
      id: "a".to_string(),
      kind: Kind::Var, 
      type_: Some(Type::Int)
    })?;

    scope.create(Item{ 
      id: "foo".to_string(),
      kind: Kind::Fun, 
      type_: Some(Type::Float)
    })?;

    assert!(scope.has("a"));
    assert!(scope.has("foo"));
    assert!(!scope.has("bar"));

    Ok(())
  }

  #[test]
  fn test_update() -> Result<()> {

    let mut scope = Scope::new();

    scope.create(Item {
      id: "a".to_string(), 
      kind: Kind::Var, 
      type_: Some(Type::Int)
    })?;

    scope.create(Item { 
      id: "foo".to_string(),
      kind: Kind::Fun, 
      type_: Some(Type::Float)
    })?;

    scope.update(Item { 
      id: "a".to_string(),
      kind: Kind::Var, 
      type_: Some(Type::Int)
    })?;

    let err = scope.update(Item { 
      id: "a".to_string(),
      kind: Kind::Var, 
      type_: Some(Type::Float)
    });

    assert_eq!(err, Err(Error::MismatchingTypes("a".to_string(), Type::Int, Type::Float)));

    let err2 = scope.update(Item { 
      id: "foo".to_string(),
      kind: Kind::Var, 
      type_: Some(Type::Float)
    });

    assert_eq!(err2, Err(Error::Unassignable("foo".to_string(), Kind::Fun)));

    Ok(())
  }

  #[test]
  fn test_remove() -> Result<()> {

    let mut scope = Scope::new();

    scope.create(Item {
      id: "a".to_string(), 
      kind: Kind::Var, 
      type_: Some(Type::Int)
    })?;

    scope.create(Item { 
      id: "foo".to_string(),
      kind: Kind::Fun, 
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
      id: "a".to_string(), 
      kind: Kind::Var, 
      type_: Some(Type::Int)
    })?;

    scope.create(Item { 
      id: "foo".to_string(),
      kind: Kind::Fun, 
      type_: Some(Type::Float)
    })?;
    
    
    let mut nested = scope.add_child();
    
    assert!(nested.get("a").is_ok());
    assert!(nested.get("foo").is_ok());
    
    nested.create(Item {
      id: "a".to_string(), 
      kind: Kind::Var, 
      type_: Some(Type::String)
    })?;

    nested.create(Item { 
      id: "foo".to_string(),
      kind: Kind::Class, 
      type_: None
    })?;

    assert_eq!(
      nested.get("a"),
      Ok(&Item {
          id: "a".to_string(), 
          kind: Kind::Var, 
          type_: Some(Type::String)
      }));
    assert_eq!(
      nested.get("foo"), 
      Ok(&Item { 
        id: "foo".to_string(),
        kind: Kind::Class, 
        type_: None
      }));

    // return ownership to the parent
    scope = nested.drop();

    assert_eq!(
      scope.get("a"), 
      Ok(&Item {
        id: "a".to_string(), 
        kind: Kind::Var, 
        type_: Some(Type::Int)
      }));

    Ok(())
  }

}