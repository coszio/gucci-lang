
use std::{collections::HashMap, fmt::Display};

use crate::parser::{Type, Expr};

type Result<T> = std::result::Result<T, Error>;

trait Typed {
  fn type_(&self) -> &Type;
}

trait Updateable<T: Typed> {
  fn set(&mut self, key: &str, value: T) -> Result<()>;
}


enum Error {
  Unassignable(String, Type, Type),
  Inexistent(String),
  Duplicate(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
          Error::Inexistent(key) => write!(f, "Error: `{}` does not exist in this scope", key),
          Error::Unassignable(key, type1, type2) => write!(f, "Error: `{}` has type {}, but is being assigned to {}", key, type1, type2),
          Error::Duplicate(key) => write!(f, "Error: `{}` already exists in this scope", key),
        }
    }
}

/// Generic directory for the different elements of the language
struct Dir<T> {
    parent: Option<Box<Self>>,
    dir: HashMap<String, T>,
}

impl<T> Dir<T> {
  fn get(&self, key: &str) -> Result<&T> {
    if let Some(t) = self.dir.get(key) {
      Ok(t)
    } else if let Some(ref parent) = self.parent {
      parent.get(key)
    } else {
      Err(Error::Inexistent(key.to_string()))
    }
  }

  fn create(&mut self, key: &str, value: T) -> Result<()> {
    if self.dir.contains_key(key) {
      Err(Error::Duplicate(key.to_string()))
    } else {
      self.dir.insert(key.to_string(), value);
      Ok(())
    }
  }

  fn has(&self, key: &str) -> bool {
    self.dir.contains_key(key) || (self.parent.is_some() && self.parent.as_ref().unwrap().has(key))
  }

  fn delete(&mut self, key: &str) -> Result<()> {
    if let Some(_) = self.dir.remove(key) {
      Ok(())
    } else if let Some(ref mut parent) = self.parent {
      parent.delete(key)
    } else {
      Err(Error::Inexistent(key.to_string()))
    }
  }
}

impl<T> Updateable<T> for Dir<T> 
where T: Typed {
  fn set(&mut self, key: &str, value: T) -> Result<()> {
    if let Some(x) = self.dir.get_mut(key) {
      if x.type_() == value.type_() {
        *x = value;
        Ok(())
      } else {
        Err(Error::Unassignable(
          key.to_string(), 
          x.type_().clone(), 
          value.type_().clone()))
      }
    } else if let Some(ref mut parent) = self.parent {
      parent.set(key, value)
    } else {
      Err(Error::Inexistent(key.to_string()))
    }
  }
}

struct Scope {
  parent: Option<Box<Self>>,
  vars: Dir<Var>,
  funcs: Dir<Fun>,
  classes: Dir<Class>,
  interfaces: Dir<Interface>,
}

struct Var {
  type_: Type,
  value: Option<Value>,
}
impl Typed for Var {
  fn type_(&self) -> &Type {
    &self.type_
  }
}

struct Value {
  type_: Type,
  expr: Expr,
}
impl Typed for Value {
  fn type_(&self) -> &Type {
    &self.type_
  }
}

struct Fun {
    ret_type: Type,
    params: Vec<Var>,
    body: Scope,
}

struct Class {
  inherits: Option<String>,
  implements: Option<String>,
  vars: Dir<Var>,
  funcs: Dir<Fun>,
}

struct Interface {
  methods: Dir<FunSignature>,
}

struct FunSignature {
  ret_type: Type,
  params: Vec<Var>,
}

