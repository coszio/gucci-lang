use std::{str::FromStr, fmt::Display};

use crate::shared::W;

use super::op_code::OpCode;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Quad {
    pub(crate) op: OpCode,
    pub(crate) arg1: String,
    pub(crate) arg2: String,
    pub(crate) arg3: String,
}

impl Quad {
    pub(crate) fn new(op: OpCode, arg1: &str, arg2: &str, arg3: &str) -> Self {
        Quad {
            op,
            arg1: arg1.to_string(),
            arg2: arg2.to_string(),
            arg3: arg3.to_string(),
        }
    }
}

impl Display for Quad {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:^W$},{:^W$},{:^W$},{:^W$}",
            self.op.to_string(),
            self.arg1,
            self.arg2,
            self.arg3
        )
    }
}

impl FromStr for Quad {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // split on commas and trim whitespace
        let mut parts = s
            .split(",")
            .map(|p| p.trim());

        let op_code = parts
            .next().unwrap()
            .parse::<OpCode>().unwrap();

        let arg1 = parts.next().unwrap();
        let arg2 = parts.next().unwrap();
        let arg3 = parts.next().unwrap();

        Ok(Quad::new(op_code, arg1, arg2, arg3))

    }
}