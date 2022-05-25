use std::{collections::HashMap, cell::RefCell, rc::Rc};

use crate::shared::quad::Quad;

mod loader;
mod memory;
mod reader;

struct Program {
    memory: Rc<RefCell<memory::Memory>>,
    vars: memory::Table,
    consts: memory::Table,
    temps: memory::Table,
    funs: HashMap<usize, usize>,
    insts: Vec<Quad>,
    ip: usize,
}

impl Program {
    fn new() -> Self {
        let mem = memory::Memory::new();
        Self {
            memory: Rc::clone(&mem),
            vars: memory::Table::new(&mem),
            consts: memory::Table::new(&mem),
            temps: memory::Table::new(&mem),
            funs: HashMap::new(),
            insts: Vec::new(),
            ip: 0,
        }
    }
}
