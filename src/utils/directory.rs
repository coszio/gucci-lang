
use std::{collections::HashMap, fmt::Display};

pub type Result<T> = std::result::Result<T, Error>;

pub trait Update<T> {
  fn update(&mut self, other: &T) -> Result<()>;
}

/// An item in the directory must implement this trait to be able to provide an id for the implementator
pub trait Key {
  fn key(&self) -> &str;
}

#[derive(Debug, PartialEq)]
pub enum Error {
  /// The given item does not exist, cannot be retrieved
  Nonexistent(String),

  /// The given item already exists, cannot be inserted
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

/// Generic directory for the different elements of the language.
/// 
/// In the specific use-case for **gucci lang** this is used in the `semantic_analysis`
/// to check for existence of elements, where each scope is represented as a new directory, which has 
/// the outer scope as its parent.
///
/// ```
/// //// this is gucci code
/// 
/// let a: int = 3;    // `a` is added in the current directory
/// 
/// while a < 10 {        // `{` creates a new directory, with the outer as its parent
/// 
///   a = a + 1;          // `a` is accessed through the parent
/// 
///   let b: char = 'k';  // `b` is added to the new directory
/// 
/// }                     // with `}` the new directory is dropped
/// ```
/// 
/// ## Example usage: 
/// ```
/// // Create a new directory
/// let dir = Dir<Item>::new();
/// 
/// let outer_item = Item::new("a", Type::Int); // Item implements `Key`
/// dir.create(outer_item);
/// 
/// // Borrow the current directory to add it as the child's parent.
/// // At this point `dir` is not usable, since the ownership has 
/// // been transferred to `sub_dir`.
/// let sub_dir = dir.add_child();
/// 
/// let inner_item = Item::new("b", Type::Char);
/// sub_dir.create(inner_item);
/// 
/// // Both elements are accessible
/// assert_eq(sub_dir.get("a"), Ok(&outer_item));
/// assert_eq(sub_dir.get("b"), Ok(&inner_item));
/// 
/// // Return ownership to `dir`
/// *dir = sub_dir.drop(); 
/// 
/// // `b` is no longer accessible
/// assert_eq(dir.get("a"), Ok(&outer_item));
/// assert_eq(dir.get("b"), Err(Error::Nonexistent("b")));
/// ```
/// 
#[derive(Debug, Clone)]
pub struct Dir<T> {
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

  /// Returns an item from the directory. If the item is not in the current directory, 
  /// it looks for it in the parent directories until there are is no parent, 
  /// in which case it returns a `NonExistent(id)` error.
  pub fn get(&self, key: &str) -> Result<&T> {
    if let Some(t) = self.dir.get(key) {
      Ok(t)
    } else if let Some(ref parent) = self.parent {
      parent.get(key)
    } else {
      Err(Error::Nonexistent(key.to_string()))
    }
  }

  /// Adds an item to the directory. If the item is already in the directory,
  /// it returns a `Duplicate(id)` error.
  /// 
  /// It allows *shadowing* the items in the parents' directories; meaning that 
  /// parent directories can already have the id that is being added, without failure.
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
