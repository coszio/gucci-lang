use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::compiler::parser::ast::Type;
use crate::shared::quad::Quad;

use super::value::Value;
use super::memory::Table;

use super::*;

#[derive(Debug)]
pub struct Program {
    memory: Rc<RefCell<memory::Memory>>,
    vars: memory::Table,
    consts: memory::Table,
    temps: memory::Table,
    funs: HashMap<usize, usize>,
    instrs: Vec<Quad>,
    ip: usize,
}

impl Program {
    pub fn new() -> Self {
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

    pub fn load(&mut self, path: &str) -> &mut Self {
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

        self
    }

    fn get_table(&self, id: &str) -> &memory::Table {
        match id {
            "v" => &self.vars,
            "c" => &self.consts,
            "t" => &self.temps,
            _ => panic!("invalid table id"),
        }
    }
    fn get_mut_table(&mut self, id: &str) -> &mut memory::Table {
        match id {
            "v" => &mut self.vars,
            "c" => &mut self.consts,
            "t" => &mut self.temps,
            _ => panic!("invalid table id"),
        }
    }

    fn execute_binop(&mut self, quad: &Quad, resolve: fn(Value, Value) -> Value) {

        let Quad {op: _, arg1: a, arg2: b, arg3: r } = quad;

        let table_id = &a[0..1];
        let a_id = &a[1..].parse::<usize>().unwrap();
        let table_a = self.get_table(table_id);
        let a = table_a.get_val(a_id);

        let b = if b.len() > 0 {
            let table_id = &b[0..1];
            let b_id = &b[1..].parse::<usize>().unwrap();
            let table_b = self.get_table(table_id);
            table_b.get_val(b_id)
        } else {
            Value::Bool(false)
        };
        
        // get result
        let res = resolve(a, b);
        
        let table_id = &r[0..1];
        let r_id = r[1..].parse::<usize>().unwrap();

        // if the operation assigns to a temp, it means it is a new temp variable
        if table_id == "t" {
            self.temps.insert(r_id, res.clone().into());
        }

        // assign
        let r_table = self.get_table(table_id);

        r_table.set_val(&r_id, res);
    }


    pub(crate) fn execute(&mut self, quad: &Quad) {
        match &quad.op {
            OpCode::Assign => self.execute_binop(quad, |a, _| a),
            OpCode::Add => self.execute_binop(quad, |a, b| a + b),
            OpCode::Sub => self.execute_binop(quad, |a, b| a - b),
            OpCode::Mul => self.execute_binop(quad, |a, b| a * b),
            OpCode::Div => self.execute_binop(quad, |a, b| a / b),
            OpCode::Gte => self.execute_binop(quad, |a, b| Value::Bool(a >= b)),
            // OpCode::Eq => {
            //     let (a, b, r_id, r_table) = self.parse_op_quad(quad);

            //     r_table.set_val(&r_id, a == b);
            // },
            // OpCode::Neq => {
            //     let (a, b, r_id, r_table) = self.parse_op_quad(quad);

            //     r_table.set_val(&r_id, a != b);
            // },
            // OpCode::Lt => {
            //     let (a, b, r_id, r_table) = self.parse_op_quad(quad);

            //     r_table.set_val(&r_id, a < b);
            // },
            // OpCode::Gt => {
            //     let (a, b, r_id, r_table) = self.parse_op_quad(quad);

            //     r_table.set_val(&r_id, a > b);
            // },
            // OpCode::Lte => {
            //     let (a, b, r_id, r_table) = self.parse_op_quad(quad);

            //     r_table.set_val(&r_id, a <= b);
            // },
            // OpCode::Gte => {
            //     let (a, b, r_id, r_table) = self.parse_op_quad(quad);

            //     r_table.set_val(&r_id, a >= b);
            // },
            OpCode::And => self.execute_binop(quad, |a, b| Value::Bool(a.to_bool() && b.to_bool())),
            OpCode::Or => self.execute_binop(quad, |a, b| Value::Bool(a.to_bool() || b.to_bool())),
            OpCode::NewVar => {
                let Quad {
                    op: _,
                    arg1: type_,
                    arg2: var,
                    arg3: _,
                } = quad;

                let table_id = &var[0..1];
                let table = self.get_mut_table(table_id);

                let id = var[1..].parse::<usize>().unwrap();

                let type_ = type_.parse::<Type>().unwrap();

                table.insert(id, type_);
            },
            OpCode::Goto => {
                let ip = quad.arg2.parse::<usize>().unwrap();

                self.ip = ip - 1;
            },
            OpCode::GotoF => {
                // get condition value
                let cond = &quad.arg1;
                let cond_table = self.get_table(&cond[0..1]);
                let cond_id = &cond[1..].parse::<usize>().unwrap();

                // jump if condition is false
                let cond = cond_table.get_val(cond_id);
                if !cond.to_bool() {
                    let ip = quad.arg2.parse::<usize>().unwrap();
                    self.ip = ip - 1;
                }
            },
            OpCode::GotoT => {
                // get condition value
                let cond = &quad.arg1;
                let cond_table = self.get_table(&cond[0..1]);
                let cond_id = &cond[1..].parse::<usize>().unwrap();

                // jump if condition is true
                let cond = cond_table.get_val(cond_id);
                if cond.to_bool() {
                    let ip = quad.arg2.parse::<usize>().unwrap();
                    self.ip = ip - 1;
                }
            },

            OpCode::Print => {
                let a = &quad.arg1;
                let a_table = self.get_table(&a[0..1]);
                let a_id = &a[1..].parse::<usize>().unwrap();

                let a = a_table.get_val(a_id);
                println!("{}", a);
            }
            OpCode::End => {
                self.ip = self.instrs.len();
            }
            other => todo!("{:?}", other),
        }
    }
    pub fn run(&mut self) {
        while self.ip < self.instrs.len() {
            let quad = self.instrs[self.ip].clone();
            self.execute(&quad);
            self.ip += 1;
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

    #[test]
    fn test_run() {
        let mut prog = Program::new();
        prog.load("src/vm/tests/test_run.bs");

        prog.run();
    }
}