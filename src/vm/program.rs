use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::compiler::parser::ast::{Type, Var};
use crate::shared::quad::Quad;

use super::memory::Table;
use super::value::Value;

use super::*;

#[derive(Debug)]
struct Call {
    vars: memory::Table,
    temps: memory::Table,
}

impl Call {
    fn new(mem: &Rc<RefCell<memory::Memory>>) -> Self {
        Self {
            vars: memory::Table::new(&mem),
            temps: memory::Table::new(&mem),
        }
    }
}
/// A program represents the virtual machine that will execute all the actions
#[derive(Debug)]
pub struct Program {
    memory: Rc<RefCell<memory::Memory>>,
    stack: Vec<Call>,
    // vars: Vec<memory::Table>,
    consts: memory::Table,
    fun_rets: memory::Table,
    params: memory::Table,
    var_stack: Vec<Vec<usize>>,
    funs: HashMap<usize, usize>,
    instrs: Vec<Quad>,
    ip_stack: Vec<usize>,
    ip: usize,
}

impl Program {
    pub fn new() -> Self {
        let mem = memory::Memory::new();
        Self {
            memory: Rc::clone(&mem),
            stack: vec![Call::new(&mem)],
            // vars: vec![memory::Table::new(&mem)],
            consts: memory::Table::new(&mem),
            fun_rets: memory::Table::new(&mem),
            params: memory::Table::new(&mem),
            var_stack: vec![vec![]],
            funs: HashMap::new(),
            instrs: Vec::new(),
            ip_stack: Vec::new(),
            ip: 0,
        }
    }

    pub fn load(&mut self, path: &str) -> &mut Self {
        
        if !path.ends_with(".gu.bs") {
            panic!("This is not a compiled file! Compiled files end in *.gu.bs")
        }

        let (funs, consts, instrs) = reader::load_obj(path);

        for fun in funs {
            self.funs.insert(fun.id, fun.pointer);
            if let Some((ret_type, id)) = fun.ret {
                self.fun_rets.alloc(id, ret_type);
            }
        }
        for const_ in consts {
            self.consts.alloc(const_.id, const_.value.clone().into());
            self.consts.set_val(&const_.id, const_.value.into());
        }

        self.instrs = instrs;

        self
    }

    fn get_table(&self, id: &str) -> &memory::Table {
        match id {
            "v" => &self.stack.last().unwrap().vars,
            "c" => &self.consts,
            "t" => &self.stack.last().unwrap().temps,
            "p" => &self.params,
            "r" => &self.fun_rets,
            _ => panic!("invalid table id"),
        }
    }
    fn get_mut_table(&mut self, id: &str) -> &mut memory::Table {
        match id {
            "v" => &mut self.stack.last_mut().unwrap().vars,
            "c" => &mut self.consts,
            "t" => &mut self.stack.last_mut().unwrap().temps,
            "p" => &mut self.params,
            "r" => &mut self.fun_rets,
            _ => panic!("invalid table id"),
        }
    }

    fn get_var(&mut self, var: &str) -> Value {
        let table_id = &var[0..1];
        let id = &var[1..].parse::<usize>().unwrap();
        let table = self.get_mut_table(table_id);
        let val = table.get_val(id);

        // if a temp or param has been used, we need to drop it
        if ["t", "p"].contains(&table_id) {
            table.remove(id);
        }

        val
    }
    fn execute_binop(&mut self, quad: &Quad, resolve: fn(Value, Value) -> Value) {
        let Quad {
            op: _,
            arg1: a,
            arg2: b,
            arg3: r,
        } = quad;

        let a = self.get_var(a);

        let b = if b.len() > 0 {
            self.get_var(b)
        } else {
            Value::Bool(false)
        };

        // get result
        let res = resolve(a, b);

        let table_id = &r[0..1];
        let r_id = r[1..].parse::<usize>().unwrap();

        // if the operation assigns to a temp, it means it is a new temp variable
        if table_id == "t" {
            self.stack.last_mut().unwrap().temps.alloc(r_id, res.clone().into());
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
            OpCode::Gte => self.execute_binop(quad, |a, b| a.gte(b)),
            OpCode::Gt => self.execute_binop(quad, |a, b| a.gt(b)),
            OpCode::Lte => self.execute_binop(quad, |a, b| a.lte(b)),
            OpCode::Lt => self.execute_binop(quad, |a, b| a.lt(b)),
            OpCode::Eq => self.execute_binop(quad, |a, b| a.eq_(b)),
            OpCode::Neq => self.execute_binop(quad, |a, b| a.neq(b)),
            OpCode::Not => self.execute_binop(quad, |a, _| !a),
            OpCode::And => self.execute_binop(quad, |a, b| a.and(b)),
            OpCode::Or => self.execute_binop(quad, |a, b| a.or(b)),
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

                table.alloc(id, type_);
                self.var_stack.last_mut().unwrap().push(id);
            }
            OpCode::Goto => {
                let ip = quad.arg2.parse::<usize>().unwrap();

                self.ip = ip - 1;
            }
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
            }
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
            }
            OpCode::GoSub => {
                // add current ip to the stack
                self.ip_stack.push(self.ip);
                self.stack.push(Call::new(&self.memory));

                // go to function
                let ip = quad.arg2.parse::<usize>().unwrap();
                self.ip = ip - 1;
            }
            OpCode::EndSub => {
                // pop ip from stack
                self.ip = self.ip_stack.pop().unwrap();
                self.stack.pop();
            }

            OpCode::Print => {
                // parse variable
                let a = &quad.arg1;
                let a_table = self.get_table(&a[0..1]);
                let a_id = &a[1..].parse::<usize>().unwrap();

                // print its value
                let a = a_table.get_val(a_id);
                println!("{}", a);
            }
            OpCode::BeginBlock => {
                // begin capturing new variables
                self.var_stack.push(Vec::new());
            }
            OpCode::EndBlock => {
                // throw away declared variables
                let block_vars = self.var_stack.pop().unwrap();
                for id in block_vars {
                    self.stack.last_mut().unwrap().vars.remove(&id);
                }
            }
            OpCode::Param => {
                // get parameter
                let a = &quad.arg1;
                let a_table = self.get_table(&a[0..1]);
                let a_id = &a[1..].parse::<usize>().unwrap();

                // get destination
                let r = &quad.arg3;
                if &r[0..1] != "p" {
                    panic!("PARAM must be assign to param table");
                }
                let r_id = r[1..].parse::<usize>().unwrap();

                // insert into parameters table
                let a = a_table.get_val(a_id);
                self.params.alloc(r_id, a.clone().into());
                self.params.set_val(&r_id, a);
            }
            OpCode::End => {
                self.ip = self.instrs.len();
                self.execute(&Quad::new(OpCode::EndBlock, "", "", ""));
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
        prog.load("src/vm/tests/test_load.gu.bs");

        println!("{:#?}", prog);

        assert_eq!(prog.funs.len(), 2);
        assert_eq!(prog.consts.len(), 6);
        assert_eq!(prog.stack.last().unwrap().vars.len(), 0);
        assert_eq!(prog.fun_rets.len(), 1);
    }

    #[test]
    fn test_run() {
        let mut prog = Program::new();
        prog.load("src/vm/tests/test_run.gu.bs");

        prog.run();
    }
}
