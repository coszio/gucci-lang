use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::compiler::parser::ast::{Literal, Type};

use super::value::Value;

/// Representation of linear memory.
/// 
/// In this case, it is generalized to any type, instead of just bits,
/// it can be the registry of a single type.
/// 
/// # Example usage
/// ```
/// // Create a new registry
/// let reg_int: Registry<i32> = Registry::new();
/// 
/// // Allocate space for a new value
/// let id = reg_int.alloc();
/// 
/// // Set the value of the allocated space
/// reg_int.set(id, 42);
/// 
/// // Get the value of the allocated space
/// assert_eq!(reg_int.get(id), 42);
/// 
/// // Free the allocated space
/// reg_int.free(id);
/// 
/// // The value of the freed space is still accessible, but when requesting
/// // a new register, this will be used first and initialized with a default value.
/// assert_eq!(reg_int.get_val(id), 42);
/// ```
#[derive(Debug)]
struct Registry<T> {
    vals: Vec<T>, // TODO: maybe switch to Array in case it is required by the project
    freed: Vec<usize>,
}
impl<T: Default> Registry<T> {
    /// Initialize an empty registry.
    fn new() -> Self {
        Self {
            vals: Vec::new(),
            freed: Vec::new(),
        }
    }

    /// Get the index of the next free register. 
    /// When there are no free registers, it expands the registry by 1.
    fn alloc(&mut self) -> usize {
        if let Some(idx) = self.freed.pop() {
            self.vals[idx] = T::default();
            idx
        } else {
            self.vals.push(T::default());
            self.vals.len() - 1
        }
    }

    /// Mark the given index as free.
    fn free(&mut self, idx: usize) {
        self.freed.push(idx);
    }

    /// Get the value at the given index.
    fn get(&self, idx: usize) -> &T {
        &self.vals[idx]
    }

    /// Update the value at the given index.
    fn set(&mut self, idx: usize, val: T) {
        self.vals[idx] = val;
    }
}


/// Representation of the memory of the virtual machine.
/// 
/// Instead of having a single memory representing bits, 
/// it is split into several registries, one for each atomic type.
/// 
#[derive(Debug)]
pub(crate) struct Memory {
    ints: Registry<i32>,
    floats: Registry<f32>,
    chars: Registry<char>,
    bools: Registry<bool>,
}
impl Memory {

    /// Initialize an empty memory. Returns a reference counted ref cell ([`Rc`]<[`RefCell`]>) to be shared across multiple tables
    pub(crate) fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            ints: Registry::new(),
            floats: Registry::new(),
            chars: Registry::new(),
            bools: Registry::new(),
        }))
    }

    /// Allocate a new register for the given type. Returns the index at the corresponding registry
    fn new_address(&mut self, type_: &Type) -> usize {
        match type_ {
            Type::Int => self.ints.alloc(),
            Type::Float => self.floats.alloc(),
            Type::Bool => self.bools.alloc(),
            Type::Char => self.chars.alloc(),
            _ => panic!("no table in memory for type {:?}", type_),
        }
    }
}

/// This is an associated struct that is used by a [`Table`] to store the type and address of a variable.
/// 
/// It is not meant to be used directly, only through the [`Table`] methods.
#[derive(Debug)]
struct Var {
    type_: Type,
    address: usize,
}

impl Var {
    /// Gets the value of the variable's address in the given memory.
    fn get(&self, mem: &Memory) -> Value {
        match self.type_ {
            Type::Int => Value::Int(*mem.ints.get(self.address)),
            Type::Float => Value::Float(*mem.floats.get(self.address)),
            Type::Char => Value::Char(*mem.chars.get(self.address)),
            Type::Bool => Value::Bool(*mem.bools.get(self.address)),
            _ => panic!("Invalid type"),
        }
    }

    /// Sets the value of the variable's address in the given memory.
    fn set(&self, mem: &mut Memory, value: Value) {
        match value {
            Value::Int(i) => mem.ints.set(self.address, i),
            Value::Float(f) => mem.floats.set(self.address, f),
            Value::Char(c) => mem.chars.set(self.address, c),
            Value::Bool(b) => mem.bools.set(self.address, b),
        }
    }

    /// Frees the value of the variable's address in the given memory.
    fn free(&self, mem: &mut Memory) {
        match self.type_ {
            Type::Int => mem.ints.free(self.address),
            Type::Float => mem.floats.free(self.address),
            Type::Char => mem.chars.free(self.address),
            Type::Bool => mem.bools.free(self.address),
            _ => panic!("Invalid type"),
        }
    }
}

/// A table is a collection of variables, and has a reference to a shared [`Memory`].
#[derive(Debug)]
pub(crate) struct Table {
    items: HashMap<usize, Var>,
    mem: Rc<RefCell<Memory>>,
}

impl Table {
    pub(crate) fn new(mem: &Rc<RefCell<Memory>>) -> Self {
        Self {
            items: HashMap::new(),
            mem: Rc::clone(mem),
        }
    }

    /// Initializes a new address for a type and assigns it to the id in the table
    pub(crate) fn alloc(&mut self, id: usize, type_: Type) {
        let address = self.mem.borrow_mut().new_address(&type_);
        self.items.insert(id, Var { type_, address });
    }

    /// Returns the value of the variable with the given id
    pub(crate) fn get_val(&self, id: &usize) -> Value {
        self.items[id].get(&self.mem.borrow())
    }

    /// Sets the value of the variable with the given id
    pub(crate) fn set_val(&self, id: &usize, value: Value) {
        self.items[id].set(&mut self.mem.borrow_mut(), value);
    }

    /// Free the memory allocated for the variable and remove the id from the table
    pub(crate) fn remove(&mut self, id: &usize) {
        self.items[id].free(&mut self.mem.borrow_mut());
        self.items.remove(&id).unwrap();
    }

    pub(crate) fn len(&self) -> usize {
        self.items.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_add_var() {
        let mem = Memory::new();
        let mut table = Table::new(&mem);

        table.alloc(0, Type::Int);
        table.alloc(1, Type::Float);
        table.alloc(2, Type::Char);
        table.alloc(3, Type::Bool);

        assert_eq!(table.get_val(&0), Value::Int(0));
        assert_eq!(table.get_val(&1), Value::Float(0.0));
        assert_eq!(table.get_val(&2), Value::Char('\0'));
        assert_eq!(table.get_val(&3), Value::Bool(false));
    }

    #[test]
    fn test_rem_var() {
        let mem = Memory::new();
        let mut table = Table::new(&mem);

        table.alloc(0, Type::Int);
        table.alloc(1, Type::Float);
        table.alloc(2, Type::Char);
        table.alloc(3, Type::Bool);

        table.remove(&0);
        table.remove(&1);
        table.remove(&2);
        table.remove(&3);

        assert_eq!(table.items.len(), 0);
    }

    #[test]
    fn test_multiple_tables() {
        let mem = Memory::new();
        let mut table = Table::new(&mem);
        let mut table2 = Table::new(&mem);

        table.alloc(10, Type::Int);
        table.alloc(11, Type::Float);
        table2.alloc(10, Type::Char);
        table2.alloc(11, Type::Bool);

        assert_eq!(table.get_val(&10), Value::Int(0));
        assert_eq!(table.get_val(&11), Value::Float(0.0));
        assert_eq!(table2.get_val(&10), Value::Char('\0'));
        assert_eq!(table2.get_val(&11), Value::Bool(false));
    }

    #[test]
    fn test_mut_values() {
        let mem = Memory::new();
        let mut table = Table::new(&mem);
        let mut table2 = Table::new(&mem);
        table.alloc(0, Type::Int);
        table2.alloc(0, Type::Float);

        table.set_val(&0, Value::Int(10));
        table2.set_val(&0, Value::Float(10.6));

        assert_eq!(table.get_val(&0), Value::Int(10));
        assert_eq!(table2.get_val(&0), Value::Float(10.6));
    }
}
