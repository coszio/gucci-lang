use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::shared::quad::Quad;

mod loader;
mod memory;
mod reader;

#[derive(Debug)]
struct Program {
    memory: Rc<RefCell<memory::Memory>>,
    vars: memory::Table,
    consts: memory::Table,
    temps: memory::Table,
    funs: HashMap<usize, usize>,
    instrs: Vec<Quad>,
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
            instrs: Vec::new(),
            ip: 0,
        }
    }

    fn load(&mut self, path: &str) {
        let (funs, consts, instrs) = reader::load_obj(path);

        for fun in funs {
            self.funs.insert(fun.id, fun.pointer);
            if let Some((ret_type, id)) = fun.ret {
                self.vars.insert(id, ret_type);
            }
        }
        for const_ in consts {
            self.consts.insert(const_.id, const_.value.clone().into());
            self.consts.set_val(const_.id, const_.value.into());
        }

        self.instrs = instrs;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load() {
        let mut prog = Program::new();
        prog.load("src/vm/tests/test_load.obj");

        println!("{:#?}", prog);

        assert_eq!(prog.funs.len(), 2);
        assert_eq!(prog.consts.len(), 6);
        assert_eq!(prog.vars.len(), 1);
        assert_eq!(prog.temps.len(), 0);
    }
}
