
use std::{collections::HashMap, fmt::Display};

pub(crate) type Result<T> = std::result::Result<T, Error>;

pub(crate) trait Update<T> {
  fn update(&mut self, other: &T) -> Result<()>;
}

pub(crate) trait Key {
  fn key(&self) -> &str;
}

#[derive(Debug, PartialEq)]
pub(crate) enum Error {
  Nonexistent(String),
  Duplicate(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Nonexistent(key) => write!(f, "Error: `{}` does not exist in this scope", key),
            Error::Duplicate(key) => write!(f, "Error: `{}` already exists in this scope", key),
        }
    }
}

/// Generic directory for the different elements of the language
#[derive(Debug, Clone)]
pub(crate) struct Dir<T> {
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

  pub fn get(&self, key: &str) -> Result<&T> {
    if let Some(t) = self.dir.get(key) {
      Ok(t)
    } else if let Some(ref parent) = self.parent {
      parent.get(key)
    } else {
      Err(Error::Nonexistent(key.to_string()))
    }
  }

  pub fn create(&mut self, item: T) -> Result<()> {
    let key = item.key();
    if self.dir.contains_key(key) {
      Err(Error::Duplicate(key.to_string()))
    } else {
      self.dir.insert(key.to_string(), item);
      Ok(())
    }
  }

  pub fn has(&self, key: &str) -> bool {
    self.dir.contains_key(key) || (self.parent.is_some() && self.parent.as_ref().unwrap().has(key))
  }

  pub fn remove(&mut self, key: &str) -> Result<()> {
    if let Some(_) = self.dir.remove(key) {
      Ok(())
    } else if let Some(ref mut parent) = self.parent {
      parent.remove(key)
    } else {
      Err(Error::Nonexistent(key.to_string()))
    }
  }

  pub fn update(&mut self, item: T) -> Result<()> where T: Update<T> {
    let key = item.key();
    if let Some(t) = self.dir.get_mut(key) {
      t.update(&item)
    } else if let Some(ref mut parent) = self.parent {
      parent.update(item)
    } else {
      Err(Error::Nonexistent(key.to_string()))
    }
  }

  pub fn add_child(&self) -> Self {
    let mut child = Dir::new();
    child.parent = Some(Box::new(self.clone()));
    child
  }

  /// Deletes current directory and returns the ownership of the parent
  pub fn drop(self) -> Self {
    let parent = *self.parent.unwrap();
    drop(self.dir);
    parent
  }
}
