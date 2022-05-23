use std::{fmt::{Display, Error, Formatter}, str::FromStr};

use crate::compiler::parser::ast::{BinOp, UnOp};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum OpCode {
    Era,
    Param,
    BeginBlock,
    EndBlock,
    Goto,
    GotoF,
    GotoT,
    Alloc,
    GoSub,
    End,

    // From UnOp
    Neg,
    Not,

    // From BinOp
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
}

impl Display for OpCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), Error> {
        match self {
            OpCode::Era => write!(f, "ERA"),
            OpCode::BeginBlock => write!(f, "BEGINBLOCK"),
            OpCode::EndBlock => write!(f, "ENDBLOCK"),
            OpCode::Goto => write!(f, "GOTO"),
            OpCode::GotoF => write!(f, "GOTOF"),
            OpCode::GotoT => write!(f, "GOTOT"),
            OpCode::Alloc => write!(f, "ALLOC"),
            OpCode::GoSub => write!(f, "GOSUB"),
            OpCode::Param => write!(f, "PARAM"),
            OpCode::End => write!(f, "END"),

            // UnOps
            OpCode::Neg => write!(f, "NEG"),
            OpCode::Not => write!(f, "NOT"),

            // BinOps
            OpCode::Add => write!(f, "ADD"),
            OpCode::Sub => write!(f, "SUB"),
            OpCode::Mul => write!(f, "MUL"),
            OpCode::Div => write!(f, "DIV"),
            OpCode::Eq => write!(f, "EQ"),
            OpCode::Neq => write!(f, "NEQ"),
            OpCode::Lt => write!(f, "LT"),
            OpCode::Gt => write!(f, "GT"),
            OpCode::Lte => write!(f, "LTE"),
            OpCode::Gte => write!(f, "GTE"),
            OpCode::And => write!(f, "AND"),
            OpCode::Or => write!(f, "OR"),
            OpCode::Assign => write!(f, "="),
        }
    }
}

impl TryFrom<BinOp> for OpCode {
    type Error = String;

    fn try_from(value: BinOp) -> Result<Self, Self::Error> {
        match value {
            BinOp::Assign => Ok(OpCode::Assign),
            // BinOp::Chain => Ok(OpCode::),
            BinOp::Add => Ok(OpCode::Add),
            BinOp::Sub => Ok(OpCode::Sub),
            BinOp::Mul => Ok(OpCode::Mul),
            BinOp::Div => Ok(OpCode::Div),
            BinOp::Eq => Ok(OpCode::Eq),
            BinOp::Ne => Ok(OpCode::Neq),
            BinOp::Lt => Ok(OpCode::Lt),
            BinOp::Gt => Ok(OpCode::Gt),
            BinOp::Lte => Ok(OpCode::Lte),
            BinOp::Gte => Ok(OpCode::Gte),
            BinOp::And => Ok(OpCode::And),
            BinOp::Or => Ok(OpCode::Or),
            _ => Err(format!("Cannot convert {} to OpCode directly", value)),
        }
    }
}

impl TryFrom<UnOp> for OpCode {
    type Error = String;

    fn try_from(value: UnOp) -> Result<Self, Self::Error> {
        match value {
            UnOp::Neg => Ok(OpCode::Neg),
            UnOp::Not => Ok(OpCode::Not),
            _ => Err(format!("Cannot convert {} to OpCode directly", value)),
        }
    }
}

impl FromStr for OpCode {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let op_code = match s {
            "ERA" => OpCode::Era,
            "PARAM" => OpCode::Param,
            "BEGINBLOCK" => OpCode::BeginBlock,
            "ENDBLOCK" => OpCode::EndBlock,
            "GOTO" => OpCode::Goto,
            "GOTOF" => OpCode::GotoF,
            "GOTOT" => OpCode::GotoT,
            "GOSUB" => OpCode::GoSub,
            "ALLOC" => OpCode::Alloc,
            "END" => OpCode::End,

            "ADD" => OpCode::Add,
            "SUB" => OpCode::Sub,
            "MUL" => OpCode::Mul,
            "DIV" => OpCode::Div,
            "EQ"  => OpCode::Eq,
            "NEQ" => OpCode::Neq,
            "LT"  => OpCode::Lt,
            "GT"  => OpCode::Gt,
            "LTE" => OpCode::Lte,
            "GTE" => OpCode::Gte,
            "AND" => OpCode::And,
            "OR"  => OpCode::Or,
            "NOT" => OpCode::Not,
            "NEG" => OpCode::Neg,
            "="   => OpCode::Assign,
            _     => return Err(format!("Invalid op code: {}", s)),
        };

        Ok(op_code)
    }
}