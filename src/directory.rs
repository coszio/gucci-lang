
use std::{collections::HashMap, fmt::Display};

use crate::parser::{Type, Expr};

type Result<T> = std::result::Result<T, Error>;

trait Update<T> {
  fn update(&mut self, other: &T) -> Result<()>;
}

trait Key {
  fn key(&self) -> &str;
}

#[derive(Debug, PartialEq)]
pub(crate) enum Error {
  MismatchingTypes(String, Type, Type),
  Nonexistent(String),
  Duplicate(String),
  Unassignable(String, Kind),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Nonexistent(key) => write!(f, "Error: `{}` does not exist in this scope", key),
            Error::MismatchingTypes(key, type1, type2) => write!(f, "Error: `{}` has type {}, but is being assigned to {}", key, type1, type2),
            Error::Duplicate(key) => write!(f, "Error: `{}` already exists in this scope", key),
            Error::Unassignable(key, kind) => write!(f, "Error: `{}` is a {}, which is not assignable", key, kind),
        }
    }
}

/// Generic directory for the different elements of the language
#[derive(Debug, Clone)]
struct Dir<T> {
    parent: Option<Box<Self>>,
    dir: HashMap<String, T>,
}

impl<T> Dir<T> 
where T: Clone + Key {
  pub fn new() -> Self {
    Dir {
      parent: None,
      dir: HashMap::new(),
    }
  }

  fn get(&self, key: &str) -> Result<&T> {
    if let Some(t) = self.dir.get(key) {
      Ok(t)
    } else if let Some(ref parent) = self.parent {
      parent.get(key)
    } else {
      Err(Error::Nonexistent(key.to_string()))
    }
  }

  fn create(&mut self, item: T) -> Result<()> {
    let key = item.key();
    if self.dir.contains_key(key) {
      Err(Error::Duplicate(key.to_string()))
    } else {
      self.dir.insert(key.to_string(), item);
      Ok(())
    }
  }

  fn has(&self, key: &str) -> bool {
    self.dir.contains_key(key) || (self.parent.is_some() && self.parent.as_ref().unwrap().has(key))
  }

  fn remove(&mut self, key: &str) -> Result<()> {
    if let Some(_) = self.dir.remove(key) {
      Ok(())
    } else if let Some(ref mut parent) = self.parent {
      parent.remove(key)
    } else {
      Err(Error::Nonexistent(key.to_string()))
    }
  }

  fn update(&mut self, item: T) -> Result<()> where T: Update<T> {
    let key = item.key();
    if let Some(t) = self.dir.get_mut(key) {
      t.update(&item)
    } else if let Some(ref mut parent) = self.parent {
      parent.update(item)
    } else {
      Err(Error::Nonexistent(key.to_string()))
    }
  }

  fn add_child(&self) -> Self {
    let mut child = Dir::new();
    child.parent = Some(Box::new(self.clone()));
    child
  }

  fn drop(self) -> Self {
    let parent = *self.parent.unwrap();
    drop(self.dir);
    parent
  }
}

type Scope = Dir<Item>;

#[derive(Debug, Clone, PartialEq)]
struct Item { 
  id: String,
  kind: Kind,
  type_: Option<Type>,
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
        }
    }
}
#[cfg(test)]
mod tests {

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