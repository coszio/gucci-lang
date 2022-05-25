use std::{
    fs::File,
    io::{BufRead, BufReader},
    str::FromStr,
};

use crate::{compiler::parser::ast::Literal, shared::quad::Quad};

pub(crate) struct Const {
    id: usize,
    value: Literal,
}

impl FromStr for Const {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split(",").map(|p| p.trim());
        let id = parts
            .next()
            .unwrap()
            .strip_prefix("c")
            .unwrap()
            .parse::<usize>()
            .unwrap();
        let value = parts.next().unwrap().parse::<Literal>()?;
        Ok(Self { id, value })
    }
}

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Function {
    id: usize,
    pointer: usize,
}

impl FromStr for Function {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split(",").map(|p| p.trim());

        let id = parts
            .next()
            .unwrap()
            .strip_prefix("f")
            .unwrap()
            .parse::<usize>()
            .unwrap();

        let pointer = parts.next().unwrap().parse::<usize>().unwrap();
        Ok(Self { id, pointer })
    }
}

fn load_constants(reader: &mut BufReader<File>) -> Vec<Const> {
    let mut consts: Vec<Const> = Vec::new();

    for line in reader.lines() {
        if line.is_err() {
            panic!("Failed to read line");
        }

        let line = line.unwrap();

        if line.contains("%%") {
            break;
        }

        consts.push(line.parse::<Const>().unwrap());
    }

    consts
}

fn load_quads(reader: &mut BufReader<File>) -> Vec<Quad> {
    let mut quads: Vec<Quad> = Vec::new();

    for line in reader.lines() {
        if line.is_err() {
            panic!("Failed to read line");
        }

        let line = line.unwrap();

        if line.contains("%%") {
            break;
        }

        let mut line = line.split(".");

        // instruction number is always equal to the index in the quads vector, so we can drop it
        let _i = line.next().unwrap();

        let quad = line.next().unwrap().parse::<Quad>().unwrap();

        quads.push(quad);
    }

    quads
}

fn load_funs(reader: &mut BufReader<File>) -> Vec<Function> {
    let mut funs: Vec<Function> = Vec::new();

    for line in reader.lines() {
        if line.is_err() {
            panic!("Failed to read line");
        }

        let line = line.unwrap();

        if line.contains("%%") {
            break;
        }

        funs.push(line.parse::<Function>().unwrap());
    }

    funs
}

pub(crate) fn load_instructions(path: &str) -> (Vec<Function>, Vec<Const>, Vec<Quad>) {
    let mut reader = BufReader::new(File::open(path).unwrap());

    let funs = load_funs(&mut reader);
    let consts = load_constants(&mut reader);
    let quads = load_quads(&mut reader);

    return (funs, consts, quads);
}

#[cfg(test)]
mod tests {

    use crate::shared::{op_code::OpCode, quad::Quad};

    use super::*;

    #[test]
    fn test_load_constants() {
        let file = File::open("src/vm/tests/constants.txt").unwrap();
        let mut reader = BufReader::new(file);
        let consts = load_constants(&mut reader);

        assert_eq!(consts.len(), 5);
        assert_eq!(consts[0].id, 0);
        assert_eq!(consts[0].value, Literal::Int(6));
        assert_eq!(consts[1].id, 1);
        assert_eq!(consts[1].value, Literal::Float(9.3));
        assert_eq!(consts[2].id, 2);
        assert_eq!(consts[2].value, Literal::String("hola mundo".to_string()));
        assert_eq!(consts[3].id, 3);
        assert_eq!(consts[3].value, Literal::Bool(false));
        assert_eq!(consts[4].id, 4);
        assert_eq!(consts[4].value, Literal::Char('c'));
    }

    #[test]
    fn test_load_quads() {
        let file = File::open("src/vm/tests/quads.txt").unwrap();
        let mut reader = BufReader::new(file);
        let quads = load_quads(&mut reader);

        assert_eq!(quads[0], Quad::new(OpCode::Mul, "c1", "c2", "t0"));
        assert_eq!(quads[1], Quad::new(OpCode::Add, "c0", "t0", "t1"));
        assert_eq!(quads[2], Quad::new(OpCode::Div, "c3", "c4", "t2"));
        assert_eq!(quads[3], Quad::new(OpCode::Gt, "c5", "c6", "t3"));
        assert_eq!(quads[4], Quad::new(OpCode::And, "t2", "t3", "t4"));
        assert_eq!(quads[5], Quad::new(OpCode::Or, "t1", "t4", "t5"));
        assert_eq!(quads[6], Quad::new(OpCode::End, "", "", ""));
    }

    #[test]
    fn test_load_funs() {
        let file = File::open("src/vm/tests/functions.txt").unwrap();
        let mut reader = BufReader::new(file);
        let funs = load_funs(&mut reader);

        assert_eq!(funs[0], Function { id: 0, pointer: 1 });
        assert_eq!(funs[1], Function { id: 1, pointer: 6 });
        assert_eq!(funs[2], Function { id: 2, pointer: 20 });
    }
}
