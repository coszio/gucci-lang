use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::shared::quad::Quad;

use super::value::Value;
use super::memory::Table;

use super::*;

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
                self.temps.insert(id, ret_type);
            }
        }
        for const_ in consts {
            self.consts.insert(const_.id, const_.value.clone().into());
            self.consts.set_val(&const_.id, const_.value.into());
        }

        self.instrs = instrs;
    }

    fn get_table(&self, id: &str) -> &memory::Table {
        match id {
            "v" => &self.vars,
            "c" => &self.consts,
            "t" => &self.temps,
            _ => panic!("invalid table id"),
        }
    }

    fn parse_op_quad(&self, quad: &Quad) -> (Value, Value, &usize, &Table) {
        let Quad {op: _, arg1: a, arg2: b, arg3: r } = quad;

        let table_id = &a[0..1];
        let table_a = self.get_table(table_id);

        let table_id = &b[0..1];
        let table_b = self.get_table(table_id);

        let table_id = &r[0..1];
        let table_r = self.get_table(table_id);

        let a_id = &a[1..].parse::<usize>().unwrap();
        let b_id = &b[1..].parse::<usize>().unwrap();
        let r_id = &r[1..].parse::<usize>().unwrap();

        // shadow args with their values
        let a = table_a.get_val(a_id);
        let b = table_b.get_val(b_id);

        (a, b, r_id, table_r)
    }

    fn execute(&self, quad: &Quad) {
        match quad.op {
            OpCode::Assign => {
                let (a, b, r_id, table_r) = self.parse_op_quad(quad);

                table_r.set_val(&r_id, a);
            },
            OpCode::Add => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a + b);
            },
            OpCode::Sub => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a - b);
            },
            OpCode::Mul => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a * b);
            },
            OpCode::Div => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a / b);
            },
            OpCode::Eq => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a == b);
            },
            OpCode::Neq => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a != b);
            },
            OpCode::Lt => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a < b);
            },
            OpCode::Gt => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a > b);
            },
            OpCode::Lte => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a <= b);
            },
            OpCode::Gte => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a >= b);
            },
            OpCode::And => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a && b);
            },
            OpCode::Or => {
                let (a, b, r_id, r_table) = self.parse_op_quad(quad);

                r_table.set_val(r_id, a || b);
            },
            _ => todo!(),
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load() {
        let mut prog = Program::new();
        prog.load("src/vm/tests/test_load.bs");

        println!("{:#?}", prog);

        assert_eq!(prog.funs.len(), 2);
        assert_eq!(prog.consts.len(), 6);
        assert_eq!(prog.vars.len(), 0);
        assert_eq!(prog.temps.len(), 1);
    }
}